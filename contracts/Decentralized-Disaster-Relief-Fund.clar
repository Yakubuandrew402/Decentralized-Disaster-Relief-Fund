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