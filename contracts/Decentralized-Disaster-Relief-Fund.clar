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

(define-constant err-insufficient-reserve (err u109))
(define-constant err-not-emergency (err u110))
(define-constant emergency-severity-threshold u5)
(define-constant reserve-percentage u10)

(define-data-var emergency-reserve-balance uint u0)
(define-data-var total-emergency-disbursements uint u0)

(define-map emergency-disbursements
  { emergency-id: uint }
  {
    disaster-id: uint,
    org-id: uint,
    amount: uint,
    timestamp: uint,
    authorized-by: principal
  }
)

(define-map emergency-counter
  { emergency-type: (string-ascii 20) }
  { counter: uint }
)

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

;; Helper function to get minimum of two numbers
(define-private (min-of (a uint) (b uint))
  (if (<= a b)
      a
      b))

(define-map matching-pools 
  { pool-id: uint }
  {
    sponsor: principal,
    disaster-id: uint,
    match-limit: uint,
    remaining-funds: uint,
    active: bool
  }
)

(define-map matching-pool-counter
  { pool-type: (string-ascii 20) }
  { counter: uint }
)

(define-public (create-matching-pool (disaster-id uint) (match-limit uint))
  (let
    (
      (current-counter (default-to { counter: u0 } (map-get? matching-pool-counter { pool-type: "global" })))
      (new-id (+ (get counter current-counter) u1))
    )
    (try! (stx-transfer? match-limit tx-sender (as-contract tx-sender)))
    (map-set matching-pool-counter { pool-type: "global" } { counter: new-id })
    (map-set matching-pools { pool-id: new-id }
      {
        sponsor: tx-sender,
        disaster-id: disaster-id,
        match-limit: match-limit,
        remaining-funds: match-limit,
        active: true
      }
    )
    (ok new-id)
  )
)
(define-public (close-matching-pool (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? matching-pools { pool-id: pool-id }) (err u404)))
    )
    (asserts! (is-eq tx-sender (get sponsor pool)) err-not-authorized)
    (asserts! (get active pool) err-invalid-status)
    (map-set matching-pools { pool-id: pool-id }
      (merge pool { active: false })
    )
    (ok true)
  )
)

(define-public (donate-with-matching (disaster-id uint) (amount uint) (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? matching-pools { pool-id: pool-id }) (err u404)))
      (matching-amount (min-of amount (get remaining-funds pool)))
    )
    (try! (donate-to-disaster disaster-id amount))
    (and (> matching-amount u0)
      (begin
        (try! (as-contract (donate-to-disaster disaster-id matching-amount)))
        (map-set matching-pools { pool-id: pool-id }
          (merge pool { remaining-funds: (- (get remaining-funds pool) matching-amount) })
        )
      )
    )
    (ok true)
  )
)


(define-map milestones
  { milestone-id: uint }
  {
    disaster-id: uint,
    org-id: uint,
    description: (string-ascii 500),
    amount: uint,
    completed: bool,
    approved: bool
  }
)

(define-map milestone-counter
  { milestone-type: (string-ascii 20) }
  { counter: uint }
)

(define-public (create-milestone (disaster-id uint) (org-id uint) (description (string-ascii 500)) (amount uint))
  (let
    (
      (current-counter (default-to { counter: u0 } (map-get? milestone-counter { milestone-type: "global" })))
      (new-id (+ (get counter current-counter) u1))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set milestone-counter { milestone-type: "global" } { counter: new-id })
    (map-set milestones { milestone-id: new-id }
      {
        disaster-id: disaster-id,
        org-id: org-id,
        description: description,
        amount: amount,
        completed: false,
        approved: false
      }
    )
    (ok new-id)
  )
)

(define-public (complete-milestone (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) (err u404)))
      (org (unwrap! (map-get? relief-organizations { org-id: (get org-id milestone) }) err-organization-not-found))
    )
    (asserts! (is-eq tx-sender (get wallet org)) err-not-authorized)
    (map-set milestones { milestone-id: milestone-id }
      (merge milestone { completed: true })
    )
    (ok true)
  )
)

(define-public (approve-and-disburse-milestone (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) (err u404)))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (get completed milestone) err-invalid-status)
    (try! (disburse-funds (get disaster-id milestone) (get org-id milestone) (get amount milestone)))
    (map-set milestones { milestone-id: milestone-id }
      (merge milestone { approved: true })
    )
    (ok true)
  )
)



(define-private (calculate-reserve-amount (donation-amount uint))
  (/ (* donation-amount reserve-percentage) u100)
)

(define-private (update-donation-with-reserve (disaster-id uint) (amount uint))
  (let
    (
      (reserve-amount (calculate-reserve-amount amount))
      (net-donation (- amount reserve-amount))
      (disaster (unwrap-panic (map-get? disasters { disaster-id: disaster-id })))
    )
    (var-set emergency-reserve-balance (+ (var-get emergency-reserve-balance) reserve-amount))
    (map-set disasters { disaster-id: disaster-id }
      (merge disaster { funds-allocated: (+ (get funds-allocated disaster) net-donation) })
    )
    net-donation
  )
)

(define-public (donate-to-disaster-with-reserve (disaster-id uint) (amount uint))
  (let
    (
      (disaster (map-get? disasters { disaster-id: disaster-id }))
      (current-counter (default-to { counter: u0 } (map-get? donation-counter { donation-type: "global" })))
      (new-id (+ (get counter current-counter) u1))
      (net-donation (calculate-reserve-amount amount))
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
    
    (update-donation-with-reserve disaster-id amount)
    (var-set total-donations (+ (var-get total-donations) amount))
    (ok new-id)
  )
)

(define-public (emergency-disburse (disaster-id uint) (org-id uint) (amount uint))
  (let
    (
      (disaster (unwrap! (map-get? disasters { disaster-id: disaster-id }) err-disaster-not-found))
      (organization (unwrap! (map-get? relief-organizations { org-id: org-id }) err-organization-not-found))
      (current-counter (default-to { counter: u0 } (map-get? emergency-counter { emergency-type: "global" })))
      (new-id (+ (get counter current-counter) u1))
      (current-reserve (var-get emergency-reserve-balance))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= (get severity disaster) emergency-severity-threshold) err-not-emergency)
    (asserts! (get active disaster) err-invalid-status)
    (asserts! (get verified organization) err-not-authorized)
    (asserts! (>= current-reserve amount) err-insufficient-reserve)
    
    (try! (as-contract (stx-transfer? amount tx-sender (get wallet organization))))
    
    (map-set emergency-counter { emergency-type: "global" } { counter: new-id })
    (map-set emergency-disbursements { emergency-id: new-id }
      {
        disaster-id: disaster-id,
        org-id: org-id,
        amount: amount,
        timestamp: stacks-block-height,
        authorized-by: tx-sender
      }
    )
    
    (var-set emergency-reserve-balance (- current-reserve amount))
    (var-set total-emergency-disbursements (+ (var-get total-emergency-disbursements) amount))
    
    (map-set relief-organizations { org-id: org-id }
      (merge organization { total-received: (+ (get total-received organization) amount) })
    )
    
    (ok new-id)
  )
)

(define-public (transfer-to-emergency-reserve (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set emergency-reserve-balance (+ (var-get emergency-reserve-balance) amount))
    (ok true)
  )
)

(define-read-only (get-emergency-reserve-balance)
  (var-get emergency-reserve-balance)
)

(define-read-only (get-total-emergency-disbursements)
  (var-get total-emergency-disbursements)
)

(define-read-only (get-emergency-disbursement-details (emergency-id uint))
  (map-get? emergency-disbursements { emergency-id: emergency-id })
)

(define-read-only (get-reserve-percentage)
  reserve-percentage
)

(define-read-only (calculate-donation-split (amount uint))
  (let
    (
      (reserve-amount (calculate-reserve-amount amount))
      (net-donation (- amount reserve-amount))
    )
    {
      reserve-amount: reserve-amount,
      net-donation: net-donation,
      total-amount: amount
    }
  )
)

(map-set emergency-counter { emergency-type: "global" } { counter: u0 })

(define-constant bronze-tier-threshold u1000000)
(define-constant silver-tier-threshold u5000000)
(define-constant gold-tier-threshold u15000000)
(define-constant platinum-tier-threshold u50000000)

(define-map donor-profiles
  { donor: principal }
  {
    total-donated: uint,
    reputation-points: uint,
    tier: (string-ascii 10),
    disasters-supported: uint,
    last-donation-block: uint,
    streak-count: uint
  }
)

(define-map donor-disaster-impact
  { donor: principal, disaster-id: uint }
  {
    total-contributed: uint,
    organizations-helped: uint,
    milestones-enabled: uint,
    impact-score: uint
  }
)

(define-map tier-benefits
  { tier: (string-ascii 10) }
  {
    voting-weight: uint,
    early-access: bool,
    fee-discount: uint,
    exclusive-campaigns: bool
  }
)

(define-private (initialize-tier-benefits)
  (begin
    (map-set tier-benefits { tier: "bronze" } { voting-weight: u1, early-access: false, fee-discount: u0, exclusive-campaigns: false })
    (map-set tier-benefits { tier: "silver" } { voting-weight: u2, early-access: true, fee-discount: u5, exclusive-campaigns: false })
    (map-set tier-benefits { tier: "gold" } { voting-weight: u3, early-access: true, fee-discount: u10, exclusive-campaigns: true })
    (map-set tier-benefits { tier: "platinum" } { voting-weight: u5, early-access: true, fee-discount: u15, exclusive-campaigns: true })
    (ok true)
  )
)

(define-private (calculate-tier (total-donated uint))
  (if (>= total-donated platinum-tier-threshold)
    "platinum"
    (if (>= total-donated gold-tier-threshold)
      "gold"
      (if (>= total-donated silver-tier-threshold)
        "silver"
        "bronze"
      )
    )
  )
)

(define-private (calculate-reputation-points (amount uint) (streak uint))
  (let
    (
      (base-points (/ amount u100000))
      (streak-bonus (if (> streak u5) (/ (* base-points u20) u100) u0))
    )
    (+ base-points streak-bonus)
  )
)

(define-private (update-donor-profile (donor principal) (amount uint) (disaster-id uint))
  (let
    (
      (existing-profile (default-to 
        { total-donated: u0, reputation-points: u0, tier: "bronze", disasters-supported: u0, last-donation-block: u0, streak-count: u0 }
        (map-get? donor-profiles { donor: donor })
      ))
      (new-total (+ (get total-donated existing-profile) amount))
      (is-consecutive (< (- stacks-block-height (get last-donation-block existing-profile)) u1440))
      (new-streak (if is-consecutive (+ (get streak-count existing-profile) u1) u1))
      (new-points (+ (get reputation-points existing-profile) (calculate-reputation-points amount new-streak)))
      (new-tier (calculate-tier new-total))
      (disaster-count (+ (get disasters-supported existing-profile) u1))
    )
    (map-set donor-profiles { donor: donor }
      {
        total-donated: new-total,
        reputation-points: new-points,
        tier: new-tier,
        disasters-supported: disaster-count,
        last-donation-block: stacks-block-height,
        streak-count: new-streak
      }
    )
    (ok true)
  )
)

(define-private (update-donor-impact (donor principal) (disaster-id uint) (amount uint))
  (let
    (
      (existing-impact (default-to 
        { total-contributed: u0, organizations-helped: u0, milestones-enabled: u0, impact-score: u0 }
        (map-get? donor-disaster-impact { donor: donor, disaster-id: disaster-id })
      ))
      (new-contributed (+ (get total-contributed existing-impact) amount))
      (new-impact-score (+ (get impact-score existing-impact) (/ amount u50000)))
    )
    (map-set donor-disaster-impact { donor: donor, disaster-id: disaster-id }
      {
        total-contributed: new-contributed,
        organizations-helped: (get organizations-helped existing-impact),
        milestones-enabled: (get milestones-enabled existing-impact),
        impact-score: new-impact-score
      }
    )
    (ok true)
  )
)

(define-public (donate-with-impact-tracking (disaster-id uint) (amount uint))
  (let
    (
      (donation-result (try! (donate-to-disaster disaster-id amount)))
    )
    (unwrap! (update-donor-profile tx-sender amount disaster-id) (err u500))
    (unwrap! (update-donor-impact tx-sender disaster-id amount) (err u501))
    (ok donation-result)
  )
)

(define-public (claim-tier-reward (reward-type (string-ascii 20)))
  (let
    (
      (profile (unwrap! (map-get? donor-profiles { donor: tx-sender }) (err u404)))
      (tier-benefit (unwrap! (map-get? tier-benefits { tier: (get tier profile) }) (err u404)))
      (reward-amount (/ (get reputation-points profile) u100))
    )
    (asserts! (> reward-amount u0) err-invalid-amount)
    (asserts! (>= (get reputation-points profile) u1000) (err u111))
    
    (map-set donor-profiles { donor: tx-sender }
      (merge profile { reputation-points: (- (get reputation-points profile) u1000) })
    )
    (ok reward-amount)
  )
)

(define-read-only (get-donor-profile (donor principal))
  (map-get? donor-profiles { donor: donor })
)

(define-read-only (get-donor-impact (donor principal) (disaster-id uint))
  (map-get? donor-disaster-impact { donor: donor, disaster-id: disaster-id })
)

(define-read-only (get-tier-benefits (tier (string-ascii 10)))
  (map-get? tier-benefits { tier: tier })
)

(define-read-only (calculate-donor-tier (total-donated uint))
  (calculate-tier total-donated)
)

(define-read-only (get-donor-voting-weight (donor principal))
  (match (map-get? donor-profiles { donor: donor })
    some-profile (let
      (
        (tier-data (unwrap-panic (map-get? tier-benefits { tier: (get tier some-profile) })))
      )
      (get voting-weight tier-data)
    )
    u0
  )
)

(initialize-tier-benefits)

;; Resource Request and Allocation System
;; Enables targeted resource fulfillment beyond monetary donations

(define-constant err-resource-not-found (err u112))
(define-constant err-invalid-priority (err u113))
(define-constant err-already-fulfilled (err u114))
(define-constant err-insufficient-quantity (err u115))
(define-constant err-invalid-location (err u116))

;; Resource categories for standardized requests
(define-constant medical-supplies "medical")
(define-constant food-water "food")
(define-constant shelter-materials "shelter")
(define-constant communication-tech "communication")
(define-constant transportation "transport")

;; Priority levels for resource allocation
(define-constant critical-priority u5)
(define-constant high-priority u4)
(define-constant medium-priority u3)
(define-constant low-priority u2)
(define-constant routine-priority u1)

(define-map resource-requests
  { request-id: uint }
  {
    disaster-id: uint,
    requesting-org: uint,
    resource-category: (string-ascii 20),
    resource-description: (string-ascii 300),
    quantity-needed: uint,
    quantity-fulfilled: uint,
    priority-level: uint,
    geographic-zone: (string-ascii 50),
    deadline-block: uint,
    status: (string-ascii 15),
    estimated-cost: uint,
    creation-block: uint
  }
)

(define-map resource-fulfillments
  { fulfillment-id: uint }
  {
    request-id: uint,
    fulfiller: principal,
    quantity-provided: uint,
    cost-amount: uint,
    delivery-method: (string-ascii 50),
    delivery-status: (string-ascii 15),
    confirmation-block: uint,
    verified-by: (optional principal)
  }
)

(define-map resource-request-counter
  { request-type: (string-ascii 20) }
  { counter: uint }
)

(define-map resource-fulfillment-counter
  { fulfillment-type: (string-ascii 20) }
  { counter: uint }
)

(define-map geographic-zones
  { zone-id: (string-ascii 50) }
  {
    zone-name: (string-ascii 100),
    active-requests: uint,
    total-fulfilled: uint,
    priority-multiplier: uint
  }
)

;; Initialize resource system counters
(define-private (initialize-resource-system)
  (begin
    (map-set resource-request-counter { request-type: "global" } { counter: u0 })
    (map-set resource-fulfillment-counter { fulfillment-type: "global" } { counter: u0 })
    (ok true)
  )
)

;; Create a new resource request
(define-public (create-resource-request 
  (disaster-id uint) 
  (resource-category (string-ascii 20)) 
  (resource-description (string-ascii 300))
  (quantity-needed uint)
  (priority-level uint)
  (geographic-zone (string-ascii 50))
  (deadline-blocks uint)
  (estimated-cost uint))
  (let
    (
      (current-counter (default-to { counter: u0 } (map-get? resource-request-counter { request-type: "global" })))
      (new-id (+ (get counter current-counter) u1))
      (org-data (unwrap! (map-get? relief-organizations { org-id: (unwrap-panic (some u1)) }) err-organization-not-found))
      (disaster (unwrap! (map-get? disasters { disaster-id: disaster-id }) err-disaster-not-found))
    )
    ;; Verify requesting organization is authorized
    (asserts! (is-eq tx-sender (get wallet org-data)) err-not-authorized)
    (asserts! (get verified org-data) err-not-authorized)
    (asserts! (get active disaster) err-invalid-status)
    (asserts! (and (>= priority-level routine-priority) (<= priority-level critical-priority)) err-invalid-priority)
    (asserts! (> quantity-needed u0) err-invalid-amount)
    (asserts! (> deadline-blocks u0) err-invalid-amount)
    
    (map-set resource-request-counter { request-type: "global" } { counter: new-id })
    (map-set resource-requests { request-id: new-id }
      {
        disaster-id: disaster-id,
        requesting-org: u1, ;; Simplified for demo - should derive from tx-sender
        resource-category: resource-category,
        resource-description: resource-description,
        quantity-needed: quantity-needed,
        quantity-fulfilled: u0,
        priority-level: priority-level,
        geographic-zone: geographic-zone,
        deadline-block: (+ stacks-block-height deadline-blocks),
        status: "open",
        estimated-cost: estimated-cost,
        creation-block: stacks-block-height
      }
    )
    
    ;; Update geographic zone statistics
    (let
      (
        (zone-data (default-to { zone-name: geographic-zone, active-requests: u0, total-fulfilled: u0, priority-multiplier: u100 }
                    (map-get? geographic-zones { zone-id: geographic-zone })))
      )
      (map-set geographic-zones { zone-id: geographic-zone }
        (merge zone-data { active-requests: (+ (get active-requests zone-data) u1) })
      )
    )
    (ok new-id)
  )
)

;; Fulfill a resource request
(define-public (fulfill-resource-request 
  (request-id uint) 
  (quantity-providing uint)
  (delivery-method (string-ascii 50)))
  (let
    (
      (request (unwrap! (map-get? resource-requests { request-id: request-id }) err-resource-not-found))
      (current-counter (default-to { counter: u0 } (map-get? resource-fulfillment-counter { fulfillment-type: "global" })))
      (new-fulfillment-id (+ (get counter current-counter) u1))
      (remaining-quantity (- (get quantity-needed request) (get quantity-fulfilled request)))
      (actual-quantity (if (<= quantity-providing remaining-quantity) quantity-providing remaining-quantity))
      (cost-per-unit (/ (get estimated-cost request) (get quantity-needed request)))
      (total-cost (* actual-quantity cost-per-unit))
    )
    (asserts! (is-eq (get status request) "open") err-invalid-status)
    (asserts! (> remaining-quantity u0) err-already-fulfilled)
    (asserts! (> quantity-providing u0) err-invalid-amount)
    (asserts! (< stacks-block-height (get deadline-block request)) (err u117)) ;; Not expired
    
    ;; Transfer payment for resources
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    
    ;; Create fulfillment record
    (map-set resource-fulfillment-counter { fulfillment-type: "global" } { counter: new-fulfillment-id })
    (map-set resource-fulfillments { fulfillment-id: new-fulfillment-id }
      {
        request-id: request-id,
        fulfiller: tx-sender,
        quantity-provided: actual-quantity,
        cost-amount: total-cost,
        delivery-method: delivery-method,
        delivery-status: "pending",
        confirmation-block: u0,
        verified-by: none
      }
    )
    
    ;; Update request with new fulfilled quantity
    (let
      (
        (new-fulfilled (+ (get quantity-fulfilled request) actual-quantity))
        (new-status (if (>= new-fulfilled (get quantity-needed request)) "completed" "partial"))
      )
      (map-set resource-requests { request-id: request-id }
        (merge request { 
          quantity-fulfilled: new-fulfilled,
          status: new-status
        })
      )
      
      ;; Update zone statistics if completed
      (if (is-eq new-status "completed")
        (let
          (
            (zone-data (unwrap-panic (map-get? geographic-zones { zone-id: (get geographic-zone request) })))
          )
          (map-set geographic-zones { zone-id: (get geographic-zone request) }
            (merge zone-data { 
              active-requests: (- (get active-requests zone-data) u1),
              total-fulfilled: (+ (get total-fulfilled zone-data) u1)
            })
          )
        )
        true
      )
    )
    (ok new-fulfillment-id)
  )
)

;; Confirm resource delivery (called by requesting organization)
(define-public (confirm-resource-delivery (fulfillment-id uint))
  (let
    (
      (fulfillment (unwrap! (map-get? resource-fulfillments { fulfillment-id: fulfillment-id }) (err u404)))
      (request (unwrap! (map-get? resource-requests { request-id: (get request-id fulfillment) }) err-resource-not-found))
      (org-data (unwrap! (map-get? relief-organizations { org-id: (get requesting-org request) }) err-organization-not-found))
    )
    (asserts! (is-eq tx-sender (get wallet org-data)) err-not-authorized)
    (asserts! (is-eq (get delivery-status fulfillment) "pending") err-invalid-status)
    
    (map-set resource-fulfillments { fulfillment-id: fulfillment-id }
      (merge fulfillment {
        delivery-status: "delivered",
        confirmation-block: stacks-block-height,
        verified-by: (some tx-sender)
      })
    )
    (ok true)
  )
)

;; Emergency priority boost for critical situations
(define-public (boost-request-priority (request-id uint))
  (let
    (
      (request (unwrap! (map-get? resource-requests { request-id: request-id }) err-resource-not-found))
      (disaster (unwrap! (map-get? disasters { disaster-id: (get disaster-id request) }) err-disaster-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= (get severity disaster) emergency-severity-threshold) err-not-emergency)
    (asserts! (< (get priority-level request) critical-priority) err-invalid-priority)
    
    (map-set resource-requests { request-id: request-id }
      (merge request { priority-level: critical-priority })
    )
    (ok true)
  )
)

;; Read-only functions for resource system
(define-read-only (get-resource-request (request-id uint))
  (map-get? resource-requests { request-id: request-id })
)

(define-read-only (get-resource-fulfillment (fulfillment-id uint))
  (map-get? resource-fulfillments { fulfillment-id: fulfillment-id })
)

(define-read-only (get-zone-statistics (zone-id (string-ascii 50)))
  (map-get? geographic-zones { zone-id: zone-id })
)

(define-read-only (get-resource-request-count)
  (get counter (default-to { counter: u0 } (map-get? resource-request-counter { request-type: "global" })))
)

(define-read-only (calculate-priority-score (request-id uint))
  (match (map-get? resource-requests { request-id: request-id })
    some-request (let
      (
        (base-priority (get priority-level some-request))
        (time-factor (if (< (- (get deadline-block some-request) stacks-block-height) u1440) u2 u1))
        (zone-multiplier (match (map-get? geographic-zones { zone-id: (get geographic-zone some-request) })
          some-zone (get priority-multiplier some-zone)
          u100
        ))
      )
      (* (* base-priority time-factor) (/ zone-multiplier u100))
    )
    u0
  )
)

;; Initialize the resource system
(initialize-resource-system)


