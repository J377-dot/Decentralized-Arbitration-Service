;; Decentralized Arbitration Service
;; A dispute resolution system for neutral arbitration

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATE (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_INVALID_ARBITRATOR (err u105))
(define-constant ERR_DISPUTE_CLOSED (err u106))
(define-constant ERR_INVALID_RULING (err u107))

;; Data Variables
(define-data-var next-dispute-id uint u1)
(define-data-var arbitration-fee uint u1000000) ;; 1 STX
(define-data-var min-arbitrator-stake uint u10000000) ;; 10 STX

;; Dispute States
(define-constant DISPUTE_OPEN u1)
(define-constant DISPUTE_IN_PROGRESS u2)
(define-constant DISPUTE_RESOLVED u3)
(define-constant DISPUTE_EXECUTED u4)

;; Data Maps
(define-map disputes
  { dispute-id: uint }
  {
    plaintiff: principal,
    defendant: principal,
    arbitrator: (optional principal),
    amount: uint,
    description: (string-utf8 500),
    evidence-hash: (optional (buff 32)),
    state: uint,
    created-at: uint,
    resolved-at: (optional uint),
    ruling: (optional uint), ;; 0 = defendant wins, 1 = plaintiff wins, 2 = split
    ruling-details: (optional (string-utf8 1000))
  }
)

(define-map arbitrators
  { arbitrator: principal }
  {
    stake: uint,
    total-cases: uint,
    successful-cases: uint,
    reputation-score: uint,
    active: bool,
    registered-at: uint
  }
)

(define-map dispute-funds
  { dispute-id: uint }
  { escrowed-amount: uint }
)

(define-map arbitrator-assignments
  { arbitrator: principal, dispute-id: uint }
  { assigned-at: uint }
)

;; Read-only functions
(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-arbitrator (arbitrator principal))
  (map-get? arbitrators { arbitrator: arbitrator })
)

(define-read-only (get-dispute-funds (dispute-id uint))
  (map-get? dispute-funds { dispute-id: dispute-id })
)

(define-read-only (get-next-dispute-id)
  (var-get next-dispute-id)
)

(define-read-only (get-arbitration-fee)
  (var-get arbitration-fee)
)

(define-read-only (get-min-arbitrator-stake)
  (var-get min-arbitrator-stake)
)

(define-read-only (calculate-reputation-score (total-cases uint) (successful-cases uint))
  (if (is-eq total-cases u0)
    u100
    (/ (* successful-cases u100) total-cases)
  )
)

;; Administrative functions
(define-public (set-arbitration-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set arbitration-fee new-fee)
    (ok true)
  )
)

(define-public (set-min-arbitrator-stake (new-stake uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set min-arbitrator-stake new-stake)
    (ok true)
  )
)

;; Arbitrator registration
(define-public (register-arbitrator)
  (let (
    (arbitrator tx-sender)
    (stake-amount (var-get min-arbitrator-stake))
  )
    (asserts! (is-none (map-get? arbitrators { arbitrator: arbitrator })) ERR_ALREADY_EXISTS)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set arbitrators
      { arbitrator: arbitrator }
      {
        stake: stake-amount,
        total-cases: u0,
        successful-cases: u0,
        reputation-score: u100,
        active: true,
        registered-at: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (deregister-arbitrator)
  (let (
    (arbitrator tx-sender)
    (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: arbitrator }) ERR_NOT_FOUND))
  )
    (map-delete arbitrators { arbitrator: arbitrator })
    (try! (as-contract (stx-transfer? (get stake arbitrator-data) tx-sender arbitrator)))
    (ok true)
  )
)

;; Dispute creation
(define-public (create-dispute (defendant principal) (amount uint) (description (string-utf8 500)))
  (let (
    (dispute-id (var-get next-dispute-id))
    (fee (var-get arbitration-fee))
    (total-amount (+ amount fee))
  )
    (asserts! (not (is-eq tx-sender defendant)) ERR_INVALID_STATE)
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))

    (map-set disputes
      { dispute-id: dispute-id }
      {
        plaintiff: tx-sender,
        defendant: defendant,
        arbitrator: none,
        amount: amount,
        description: description,
        evidence-hash: none,
        state: DISPUTE_OPEN,
        created-at: stacks-block-height,
        resolved-at: none,
        ruling: none,
        ruling-details: none
      }
    )

    (map-set dispute-funds
      { dispute-id: dispute-id }
      { escrowed-amount: total-amount }
    )

    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

;; Arbitrator selection
(define-public (accept-arbitration (dispute-id uint))
  (let (
    (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_NOT_FOUND))
    (arbitrator tx-sender)
    (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: arbitrator }) ERR_INVALID_ARBITRATOR))
  )
    (asserts! (is-eq (get state dispute-data) DISPUTE_OPEN) ERR_INVALID_STATE)
    (asserts! (is-none (get arbitrator dispute-data)) ERR_INVALID_STATE)
    (asserts! (get active arbitrator-data) ERR_INVALID_ARBITRATOR)

    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data { arbitrator: (some arbitrator), state: DISPUTE_IN_PROGRESS })
    )

    (map-set arbitrator-assignments
      { arbitrator: arbitrator, dispute-id: dispute-id }
      { assigned-at: stacks-block-height }
    )

    (ok true)
  )
)

;; Evidence submission
(define-public (submit-evidence (dispute-id uint) (evidence-hash (buff 32)))
  (let (
    (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_NOT_FOUND))
  )
    (asserts! (or (is-eq tx-sender (get plaintiff dispute-data))
                  (is-eq tx-sender (get defendant dispute-data))) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get state dispute-data) DISPUTE_IN_PROGRESS) ERR_INVALID_STATE)

    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data { evidence-hash: (some evidence-hash) })
    )

    (ok true)
  )
)

;; Ruling submission
(define-public (submit-ruling (dispute-id uint) (ruling uint) (ruling-details (string-utf8 1000)))
  (let (
    (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_NOT_FOUND))
    (arbitrator (unwrap! (get arbitrator dispute-data) ERR_INVALID_STATE))
  )
    (asserts! (is-eq tx-sender arbitrator) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get state dispute-data) DISPUTE_IN_PROGRESS) ERR_INVALID_STATE)
    (asserts! (<= ruling u2) ERR_INVALID_RULING)

    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data {
        state: DISPUTE_RESOLVED,
        resolved-at: (some stacks-block-height),
        ruling: (some ruling),
        ruling-details: (some ruling-details)
      })
    )

    ;; Update arbitrator stats
    (let (
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: arbitrator }) ERR_NOT_FOUND))
      (new-total-cases (+ (get total-cases arbitrator-data) u1))
      (new-successful-cases (+ (get successful-cases arbitrator-data) u1))
      (new-reputation (calculate-reputation-score new-total-cases new-successful-cases))
    )
      (map-set arbitrators
        { arbitrator: arbitrator }
        (merge arbitrator-data {
          total-cases: new-total-cases,
          successful-cases: new-successful-cases,
          reputation-score: new-reputation
        })
      )
    )

    (ok true)
  )
)

;; Ruling execution
(define-public (execute-ruling (dispute-id uint))
  (let (
    (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_NOT_FOUND))
    (funds-data (unwrap! (map-get? dispute-funds { dispute-id: dispute-id }) ERR_NOT_FOUND))
    (ruling (unwrap! (get ruling dispute-data) ERR_INVALID_STATE))
    (arbitrator (unwrap! (get arbitrator dispute-data) ERR_INVALID_STATE))
    (amount (get amount dispute-data))
    (fee (var-get arbitration-fee))
    (total-escrowed (get escrowed-amount funds-data))
  )
    (asserts! (is-eq (get state dispute-data) DISPUTE_RESOLVED) ERR_INVALID_STATE)

    ;; Pay arbitrator fee
    (try! (as-contract (stx-transfer? fee tx-sender arbitrator)))

    ;; Execute ruling
    (if (is-eq ruling u0)
      ;; Defendant wins - return disputed amount to defendant
      (try! (as-contract (stx-transfer? amount tx-sender (get defendant dispute-data))))
      (if (is-eq ruling u1)
        ;; Plaintiff wins - send disputed amount to plaintiff
        (try! (as-contract (stx-transfer? amount tx-sender (get plaintiff dispute-data))))
        ;; Split decision - divide amount between parties
        (begin
          (try! (as-contract (stx-transfer? (/ amount u2) tx-sender (get plaintiff dispute-data))))
          (try! (as-contract (stx-transfer? (/ amount u2) tx-sender (get defendant dispute-data))))
        )
      )
    )

    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data { state: DISPUTE_EXECUTED })
    )

    (map-delete dispute-funds { dispute-id: dispute-id })
    (ok true)
  )
)
