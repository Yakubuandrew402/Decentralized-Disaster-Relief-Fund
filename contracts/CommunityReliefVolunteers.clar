;; Community Relief Volunteers System
;; Coordinates volunteer efforts and skill-based matching for disaster relief operations

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u300))
(define-constant err-volunteer-not-found (err u301))
(define-constant err-already-registered (err u302))
(define-constant err-invalid-skill (err u303))
(define-constant err-assignment-not-found (err u304))
(define-constant err-already-completed (err u305))
(define-constant err-insufficient-hours (err u306))

;; Skill categories for volunteers
(define-constant skill-medical "medical")
(define-constant skill-logistics "logistics") 
(define-constant skill-technical "technical")
(define-constant skill-construction "construction")
(define-constant skill-communications "communications")
(define-constant skill-translation "translation")
(define-constant skill-childcare "childcare")
(define-constant skill-counseling "counseling")

;; Verification levels
(define-constant unverified-level u1)
(define-constant basic-verified-level u2)
(define-constant experienced-level u3)
(define-constant expert-level u4)

;; Data variables
(define-data-var volunteer-counter uint u0)
(define-data-var assignment-counter uint u0)
(define-data-var total-volunteer-hours uint u0)

;; Maps for volunteer management
(define-map volunteer-profiles
    principal ;; volunteer wallet
    {
        name: (string-ascii 50),
        primary-skill: (string-ascii 20),
        secondary-skills: (string-ascii 100), ;; comma separated
        location: (string-ascii 50),
        availability-hours: uint, ;; hours per week
        verification-level: uint,
        total-hours-served: uint,
        reputation-score: uint,
        emergency-contact: (string-ascii 50),
        registration-block: uint
    }
)

(define-map volunteer-assignments
    uint ;; assignment-id
    {
        disaster-id: uint,
        volunteer: principal,
        skill-required: (string-ascii 20),
        task-description: (string-ascii 200),
        estimated-hours: uint,
        actual-hours: uint,
        status: (string-ascii 15), ;; "assigned", "active", "completed", "cancelled"
        assigned-block: uint,
        completed-block: uint,
        verified-by: (optional principal)
    }
)

(define-map skill-demand
    { disaster-id: uint, skill: (string-ascii 20) }
    {
        volunteers-needed: uint,
        volunteers-assigned: uint,
        priority-level: uint,
        coordinator: principal
    }
)

(define-map volunteer-rewards
    principal ;; volunteer wallet
    {
        total-tokens-earned: uint,
        last-reward-block: uint,
        reward-multiplier: uint ;; based on verification level
    }
)

;; Register as a volunteer
(define-public (register-volunteer 
    (name (string-ascii 50))
    (primary-skill (string-ascii 20))
    (secondary-skills (string-ascii 100))
    (location (string-ascii 50))
    (availability-hours uint)
    (emergency-contact (string-ascii 50)))
    (begin
        ;; Check if already registered
        (asserts! (is-none (map-get? volunteer-profiles tx-sender)) err-already-registered)
        
        ;; Validate primary skill
        (asserts! (or (is-eq primary-skill skill-medical)
                     (or (is-eq primary-skill skill-logistics)
                         (or (is-eq primary-skill skill-technical)
                             (or (is-eq primary-skill skill-construction)
                                 (or (is-eq primary-skill skill-communications)
                                     (or (is-eq primary-skill skill-translation)
                                         (or (is-eq primary-skill skill-childcare)
                                             (is-eq primary-skill skill-counseling)))))))) err-invalid-skill)
        
        ;; Register volunteer profile
        (map-set volunteer-profiles tx-sender
            {
                name: name,
                primary-skill: primary-skill,
                secondary-skills: secondary-skills,
                location: location,
                availability-hours: availability-hours,
                verification-level: unverified-level,
                total-hours-served: u0,
                reputation-score: u100, ;; starting score
                emergency-contact: emergency-contact,
                registration-block: stacks-block-height
            }
        )
        
        ;; Initialize reward tracking
        (map-set volunteer-rewards tx-sender
            {
                total-tokens-earned: u0,
                last-reward-block: u0,
                reward-multiplier: u1
            }
        )
        
        (var-set volunteer-counter (+ (var-get volunteer-counter) u1))
        (ok true)
    )
)

;; Verify a volunteer (admin function)
(define-public (verify-volunteer (volunteer principal) (verification-level uint))
    (let
        (
            (profile (unwrap! (map-get? volunteer-profiles volunteer) err-volunteer-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (and (>= verification-level unverified-level) (<= verification-level expert-level)) err-invalid-skill)
        
        ;; Update verification level
        (map-set volunteer-profiles volunteer
            (merge profile { verification-level: verification-level })
        )
        
        ;; Update reward multiplier based on verification
        (let
            (
                (rewards (unwrap-panic (map-get? volunteer-rewards volunteer)))
                (new-multiplier (if (is-eq verification-level expert-level) u4
                               (if (is-eq verification-level experienced-level) u3
                               (if (is-eq verification-level basic-verified-level) u2 u1))))
            )
            (map-set volunteer-rewards volunteer
                (merge rewards { reward-multiplier: new-multiplier })
            )
        )
        (ok true)
    )
)

;; Create skill demand for a disaster
(define-public (create-skill-demand 
    (disaster-id uint)
    (skill (string-ascii 20))
    (volunteers-needed uint)
    (priority-level uint))
    (begin
        ;; Only contract owner can create skill demands (would integrate with main contract)
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        
        (map-set skill-demand { disaster-id: disaster-id, skill: skill }
            {
                volunteers-needed: volunteers-needed,
                volunteers-assigned: u0,
                priority-level: priority-level,
                coordinator: tx-sender
            }
        )
        (ok true)
    )
)

;; Assign volunteer to a task
(define-public (assign-volunteer-task
    (disaster-id uint)
    (volunteer principal)
    (skill-required (string-ascii 20))
    (task-description (string-ascii 200))
    (estimated-hours uint))
    (let
        (
            (assignment-id (+ (var-get assignment-counter) u1))
            (profile (unwrap! (map-get? volunteer-profiles volunteer) err-volunteer-not-found))
            (demand (map-get? skill-demand { disaster-id: disaster-id, skill: skill-required }))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (>= (get verification-level profile) basic-verified-level) err-not-authorized)
        
        ;; Check if volunteer has required skill (simplified - check primary skill only)
        (asserts! (is-eq (get primary-skill profile) skill-required) err-invalid-skill)
        
        ;; Create assignment
        (map-set volunteer-assignments assignment-id
            {
                disaster-id: disaster-id,
                volunteer: volunteer,
                skill-required: skill-required,
                task-description: task-description,
                estimated-hours: estimated-hours,
                actual-hours: u0,
                status: "assigned",
                assigned-block: stacks-block-height,
                completed-block: u0,
                verified-by: none
            }
        )
        
        ;; Update skill demand if exists
        (match demand
            some-demand (map-set skill-demand { disaster-id: disaster-id, skill: skill-required }
                            (merge some-demand { volunteers-assigned: (+ (get volunteers-assigned some-demand) u1) }))
            true
        )
        
        (var-set assignment-counter assignment-id)
        (ok assignment-id)
    )
)

;; Complete volunteer assignment and claim hours
(define-public (complete-assignment (assignment-id uint) (actual-hours uint))
    (let
        (
            (assignment (unwrap! (map-get? volunteer-assignments assignment-id) err-assignment-not-found))
            (profile (unwrap! (map-get? volunteer-profiles (get volunteer assignment)) err-volunteer-not-found))
        )
        (asserts! (is-eq tx-sender (get volunteer assignment)) err-not-authorized)
        (asserts! (is-eq (get status assignment) "assigned") err-already-completed)
        (asserts! (> actual-hours u0) err-insufficient-hours)
        
        ;; Update assignment status
        (map-set volunteer-assignments assignment-id
            (merge assignment {
                actual-hours: actual-hours,
                status: "completed",
                completed-block: stacks-block-height
            })
        )
        
        ;; Update volunteer profile with hours
        (map-set volunteer-profiles (get volunteer assignment)
            (merge profile {
                total-hours-served: (+ (get total-hours-served profile) actual-hours),
                reputation-score: (+ (get reputation-score profile) (/ actual-hours u2)) ;; 0.5 points per hour
            })
        )
        
        ;; Update total volunteer hours
        (var-set total-volunteer-hours (+ (var-get total-volunteer-hours) actual-hours))
        
        (ok actual-hours)
    )
)

;; Verify completed assignment and award tokens
(define-public (verify-and-reward-assignment (assignment-id uint))
    (let
        (
            (assignment (unwrap! (map-get? volunteer-assignments assignment-id) err-assignment-not-found))
            (volunteer (get volunteer assignment))
            (rewards (unwrap! (map-get? volunteer-rewards volunteer) err-volunteer-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (is-eq (get status assignment) "completed") err-not-authorized)
        
        (let
            (
                (reward-tokens (* (get actual-hours assignment) (get reward-multiplier rewards) u10)) ;; 10 base tokens per hour
            )
            ;; Update assignment as verified
            (map-set volunteer-assignments assignment-id
                (merge assignment { verified-by: (some tx-sender) })
            )
            
            ;; Award tokens (simplified - in production would transfer actual tokens)
            (map-set volunteer-rewards volunteer
                (merge rewards {
                    total-tokens-earned: (+ (get total-tokens-earned rewards) reward-tokens),
                    last-reward-block: stacks-block-height
                })
            )
            
            (ok reward-tokens)
        )
    )
)

;; Read-only functions

(define-read-only (get-volunteer-profile (volunteer principal))
    (map-get? volunteer-profiles volunteer)
)

(define-read-only (get-volunteer-assignment (assignment-id uint))
    (map-get? volunteer-assignments assignment-id)
)

(define-read-only (get-skill-demand (disaster-id uint) (skill (string-ascii 20)))
    (map-get? skill-demand { disaster-id: disaster-id, skill: skill })
)

(define-read-only (get-volunteer-rewards (volunteer principal))
    (map-get? volunteer-rewards volunteer)
)

(define-read-only (get-total-volunteer-hours)
    (var-get total-volunteer-hours)
)

(define-read-only (get-volunteer-count)
    (var-get volunteer-counter)
)

(define-read-only (find-qualified-volunteers (skill (string-ascii 20)) (min-verification-level uint))
    ;; Simplified search - in real implementation would return list of qualified volunteers
    (ok { 
        skill-searched: skill,
        min-level: min-verification-level,
        search-block: stacks-block-height
    })
)
