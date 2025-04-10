;; Decentralized Disaster Relief Fund
;; A transparent system for disaster relief fund management and distribution

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-disaster-not-found (err u104))
(define-constant err-disaster-already-exists (err u105))
(define-constant err-organization-not-found (err u106))
(define-constant err-organization-already-exists (err u107))
(define-constant err-invalid-status (err u108))

(define-data-var total-donations uint u0)
(define-data-var total-disbursements uint u0)

(define-map disasters
  { disaster-id: uint }
  {
    name: (string-ascii 100),
    location: (string-ascii 100),
    description: (string-ascii 500),
    severity: uint,
    active: bool,
    funds-allocated: uint,
    funds-disbursed: uint,
    creation-time: uint
  }
)

(define-map disaster-counter
  { disaster-type: (string-ascii 20) }
  { counter: uint }
)

(define-map relief-organizations
  { org-id: uint }
  {
    name: (string-ascii 100),
    wallet: principal,
    verified: bool,
    total-received: uint
  }
)

(define-map org-counter
  { org-type: (string-ascii 20) }
  { counter: uint }
)

(define-map donations
  { donation-id: uint }
  {
    donor: principal,
    amount: uint,
    disaster-id: uint,
    timestamp: uint
  }
)

(define-map donation-counter
  { donation-type: (string-ascii 20) }
  { counter: uint }
)

(define-map disbursements
  { disbursement-id: uint }
  {
    disaster-id: uint,
    org-id: uint,
    amount: uint,
    timestamp: uint
  }
)

(define-map disbursement-counter
  { disbursement-type: (string-ascii 20) }
  { counter: uint }
)

;; Initialize counters
(define-private (initialize-counters)
  (begin
    (map-set disaster-counter { disaster-type: "global" } { counter: u0 })
    (map-set org-counter { org-type: "global" } { counter: u0 })
    (map-set donation-counter { donation-type: "global" } { counter: u0 })
    (map-set disbursement-counter { disbursement-type: "global" } { counter: u0 })
    (ok true)
  )
)

;; Register a new disaster
(define-public (register-disaster (name (string-ascii 100)) (location (string-ascii 100)) (description (string-ascii 500)) (severity uint))
  (let
    (
      (current-counter (default-to { counter: u0 } (map-get? disaster-counter { disaster-type: "global" })))
      (new-id (+ (get counter current-counter) u1))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set disaster-counter { disaster-type: "global" } { counter: new-id })
    (map-set disasters { disaster-id: new-id }
      {
        name: name,
        location: location,
        description: description,
        severity: severity,
        active: true,
        funds-allocated: u0,
        funds-disbursed: u0,
        creation-time: stacks-block-height
      }
    )
    (ok new-id)
  )
)

;; Register a relief organization
(define-public (register-organization (name (string-ascii 100)) (org-wallet principal))
  (let
    (
      (current-counter (default-to { counter: u0 } (map-get? org-counter { org-type: "global" })))
      (new-id (+ (get counter current-counter) u1))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set org-counter { org-type: "global" } { counter: new-id })
    (map-set relief-organizations { org-id: new-id }
      {
        name: name,
        wallet: org-wallet,
        verified: true,
        total-received: u0
      }
    )
    (ok new-id)
  )
)

;; Donate to a specific disaster
(define-public (donate-to-disaster (disaster-id uint) (amount uint))
  (let
    (
      (disaster (map-get? disasters { disaster-id: disaster-id }))
      (current-counter (default-to { counter: u0 } (map-get? donation-counter { donation-type: "global" })))
      (new-id (+ (get counter current-counter) u1))
    )
    (asserts! (not (is-none disaster)) err-disaster-not-found)
    (asserts! (get active (unwrap-panic disaster)) err-invalid-status)
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set donation-counter { donation-type: "global" } { counter: new-id })
    (map-set donations { donation-id: new-id }
      {
        donor: tx-sender,
        amount: amount,
        disaster-id: disaster-id,
        timestamp: stacks-block-height
      }
    )
    
    (map-set disasters { disaster-id: disaster-id }
      (merge (unwrap-panic disaster)
        { funds-allocated: (+ (get funds-allocated (unwrap-panic disaster)) amount) }
      )
    )
    
    (var-set total-donations (+ (var-get total-donations) amount))
    (ok new-id)
  )
)

;; Disburse funds to a relief organization for a specific disaster
(define-public (disburse-funds (disaster-id uint) (org-id uint) (amount uint))
  (let
    (
      (disaster (map-get? disasters { disaster-id: disaster-id }))
      (organization (map-get? relief-organizations { org-id: org-id }))
      (current-counter (default-to { counter: u0 } (map-get? disbursement-counter { disbursement-type: "global" })))
      (new-id (+ (get counter current-counter) u1))
      (available-funds (- (get funds-allocated (unwrap-panic disaster)) (get funds-disbursed (unwrap-panic disaster))))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (is-none disaster)) err-disaster-not-found)
    (asserts! (not (is-none organization)) err-organization-not-found)
    (asserts! (get active (unwrap-panic disaster)) err-invalid-status)
    (asserts! (>= available-funds amount) err-insufficient-funds)
    
    (try! (as-contract (stx-transfer? amount tx-sender (get wallet (unwrap-panic organization)))))
    
    (map-set disbursement-counter { disbursement-type: "global" } { counter: new-id })
    (map-set disbursements { disbursement-id: new-id }
      {
        disaster-id: disaster-id,
        org-id: org-id,
        amount: amount,
        timestamp: stacks-block-height
      }
    )
    
    (map-set disasters { disaster-id: disaster-id }
      (merge (unwrap-panic disaster)
        { funds-disbursed: (+ (get funds-disbursed (unwrap-panic disaster)) amount) }
      )
    )
    
    (map-set relief-organizations { org-id: org-id }
      (merge (unwrap-panic organization)
        { total-received: (+ (get total-received (unwrap-panic organization)) amount) }
      )
    )
    
    (var-set total-disbursements (+ (var-get total-disbursements) amount))
    (ok new-id)
  )
)

;; Close a disaster (mark as inactive)
(define-public (close-disaster (disaster-id uint))
  (let
    (
      (disaster (map-get? disasters { disaster-id: disaster-id }))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (is-none disaster)) err-disaster-not-found)
    
    (map-set disasters { disaster-id: disaster-id }
      (merge (unwrap-panic disaster) { active: false })
    )
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-disaster-details (disaster-id uint))
  (map-get? disasters { disaster-id: disaster-id })
)

(define-read-only (get-organization-details (org-id uint))
  (map-get? relief-organizations { org-id: org-id })
)

(define-read-only (get-donation-details (donation-id uint))
  (map-get? donations { donation-id: donation-id })
)

(define-read-only (get-disbursement-details (disbursement-id uint))
  (map-get? disbursements { disbursement-id: disbursement-id })
)

(define-read-only (get-total-donations)
  (var-get total-donations)
)

(define-read-only (get-total-disbursements)
  (var-get total-disbursements)
)

(define-read-only (get-disaster-count)
  (get counter (default-to { counter: u0 } (map-get? disaster-counter { disaster-type: "global" })))
)

(define-read-only (get-organization-count)
  (get counter (default-to { counter: u0 } (map-get? org-counter { org-type: "global" })))
)

;; Initialize contract
(initialize-counters)
