;; Certification Mentorship System
;; Connects certified professionals with certification seekers for structured guidance

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u600))
(define-constant ERR_MENTOR_EXISTS (err u601))
(define-constant ERR_MENTOR_NOT_FOUND (err u602))
(define-constant ERR_INSUFFICIENT_CERTS (err u603))
(define-constant ERR_REQUEST_NOT_FOUND (err u604))
(define-constant ERR_SESSION_NOT_FOUND (err u605))
(define-constant ERR_INVALID_RATING (err u606))
(define-constant ERR_ALREADY_RATED (err u607))
(define-constant ERR_INVALID_STATUS (err u608))
(define-constant ERR_MENTORSHIP_FULL (err u609))

;; Data variables
(define-data-var next-request-id uint u1)
(define-data-var next-session-id uint u1)
(define-data-var min-certifications-required uint u2)
(define-data-var max-mentees-per-mentor uint u5)

;; Mentor profiles with expertise and availability
(define-map mentor-profiles
    principal
    {
        expertise-areas: (list 5 (string-ascii 30)),
        hourly-rate: uint,
        max-mentees: uint,
        current-mentees: uint,
        total-sessions: uint,
        avg-rating: uint,
        bio: (string-ascii 200),
        availability: (string-ascii 50),
        registered-at: uint,
        status: (string-ascii 10)
    }
)

;; Mentorship requests from seekers
(define-map mentorship-requests
    uint
    {
        seeker: principal,
        expertise-needed: (string-ascii 30),
        goal-description: (string-ascii 150),
        preferred-schedule: (string-ascii 50),
        budget-range: uint,
        status: (string-ascii 15),
        requested-at: uint,
        matched-mentor: (optional principal)
    }
)

;; Active mentorship relationships
(define-map active-mentorships
    { mentor: principal, mentee: principal }
    {
        start-date: uint,
        sessions-completed: uint,
        total-sessions-planned: uint,
        current-goal: (string-ascii 150),
        progress-notes: (string-ascii 200),
        status: (string-ascii 15)
    }
)

;; Individual mentorship sessions
(define-map mentorship-sessions
    uint
    {
        mentor: principal,
        mentee: principal,
        session-type: (string-ascii 20),
        scheduled-for: uint,
        duration-minutes: uint,
        session-notes: (optional (string-ascii 200)),
        completed: bool,
        session-rating: (optional uint)
    }
)

;; Mentor ratings and feedback
(define-map mentor-ratings
    { mentor: principal, rater: principal }
    {
        rating: uint,
        feedback: (string-ascii 150),
        rated-at: uint
    }
)

;; Track mentor qualifications
(define-map mentor-certifications
    principal
    (list 10 uint)
)

;; Register as a mentor with certification requirements
(define-public (register-mentor
    (expertise-areas (list 5 (string-ascii 30)))
    (hourly-rate uint)
    (bio (string-ascii 200))
    (availability (string-ascii 50))
    (cert-ids (list 10 uint)))
    
    (let ((existing-mentor (map-get? mentor-profiles tx-sender)))
        (begin
            ;; Check mentor doesn't already exist
            (asserts! (is-none existing-mentor) ERR_MENTOR_EXISTS)
            ;; Validate minimum certification requirements
            (asserts! (>= (len cert-ids) (var-get min-certifications-required)) ERR_INSUFFICIENT_CERTS)
            ;; Verify all provided certifications belong to the user
            (asserts! (verify-mentor-certifications cert-ids) ERR_NOT_AUTHORIZED)
            
            ;; Create mentor profile
            (map-set mentor-profiles tx-sender
                {
                    expertise-areas: expertise-areas,
                    hourly-rate: hourly-rate,
                    max-mentees: (var-get max-mentees-per-mentor),
                    current-mentees: u0,
                    total-sessions: u0,
                    avg-rating: u0,
                    bio: bio,
                    availability: availability,
                    registered-at: stacks-block-height,
                    status: "active"
                })
            
            ;; Store mentor certifications
            (map-set mentor-certifications tx-sender cert-ids)
            (ok true)
        )
    )
)

;; Submit mentorship request
(define-public (request-mentorship
    (expertise-needed (string-ascii 30))
    (goal-description (string-ascii 150))
    (preferred-schedule (string-ascii 50))
    (budget-range uint))
    
    (let ((request-id (var-get next-request-id)))
        (begin
            ;; Create mentorship request
            (map-set mentorship-requests request-id
                {
                    seeker: tx-sender,
                    expertise-needed: expertise-needed,
                    goal-description: goal-description,
                    preferred-schedule: preferred-schedule,
                    budget-range: budget-range,
                    status: "open",
                    requested-at: stacks-block-height,
                    matched-mentor: none
                })
            
            (var-set next-request-id (+ request-id u1))
            (ok request-id)
        )
    )
)

;; Mentor accepts mentorship request
(define-public (accept-mentorship (request-id uint))
    (let ((request (unwrap! (map-get? mentorship-requests request-id) ERR_REQUEST_NOT_FOUND))
          (mentor (unwrap! (map-get? mentor-profiles tx-sender) ERR_MENTOR_NOT_FOUND)))
        (begin
            ;; Verify mentor is active and has capacity
            (asserts! (is-eq (get status mentor) "active") ERR_INVALID_STATUS)
            (asserts! (< (get current-mentees mentor) (get max-mentees mentor)) ERR_MENTORSHIP_FULL)
            (asserts! (is-eq (get status request) "open") ERR_INVALID_STATUS)
            
            ;; Update request with matched mentor
            (map-set mentorship-requests request-id
                (merge request {
                    status: "matched",
                    matched-mentor: (some tx-sender)
                }))
            
            ;; Create active mentorship relationship
            (map-set active-mentorships 
                { mentor: tx-sender, mentee: (get seeker request) }
                {
                    start-date: stacks-block-height,
                    sessions-completed: u0,
                    total-sessions-planned: u4,
                    current-goal: (get goal-description request),
                    progress-notes: "",
                    status: "active"
                })
            
            ;; Update mentor current mentee count
            (map-set mentor-profiles tx-sender
                (merge mentor { current-mentees: (+ (get current-mentees mentor) u1) }))
            
            (ok true)
        )
    )
)

;; Schedule mentorship session
(define-public (schedule-session
    (mentee principal)
    (session-type (string-ascii 20))
    (scheduled-for uint)
    (duration-minutes uint))
    
    (let ((session-id (var-get next-session-id))
          (mentorship (unwrap! (map-get? active-mentorships { mentor: tx-sender, mentee: mentee }) ERR_REQUEST_NOT_FOUND)))
        (begin
            ;; Verify active mentorship exists
            (asserts! (is-eq (get status mentorship) "active") ERR_INVALID_STATUS)
            
            ;; Create session record
            (map-set mentorship-sessions session-id
                {
                    mentor: tx-sender,
                    mentee: mentee,
                    session-type: session-type,
                    scheduled-for: scheduled-for,
                    duration-minutes: duration-minutes,
                    session-notes: none,
                    completed: false,
                    session-rating: none
                })
            
            (var-set next-session-id (+ session-id u1))
            (ok session-id)
        )
    )
)

;; Complete mentorship session with notes
(define-public (complete-session 
    (session-id uint)
    (session-notes (string-ascii 200)))
    
    (let ((session (unwrap! (map-get? mentorship-sessions session-id) ERR_SESSION_NOT_FOUND))
          (mentorship (unwrap! (map-get? active-mentorships 
                                { mentor: (get mentor session), mentee: (get mentee session) }) 
                               ERR_REQUEST_NOT_FOUND)))
        (begin
            ;; Only mentor can complete sessions
            (asserts! (is-eq tx-sender (get mentor session)) ERR_NOT_AUTHORIZED)
            (asserts! (not (get completed session)) ERR_INVALID_STATUS)
            
            ;; Update session as completed
            (map-set mentorship-sessions session-id
                (merge session {
                    completed: true,
                    session-notes: (some session-notes)
                }))
            
            ;; Update mentorship relationship
            (map-set active-mentorships 
                { mentor: (get mentor session), mentee: (get mentee session) }
                (merge mentorship {
                    sessions-completed: (+ (get sessions-completed mentorship) u1),
                    progress-notes: session-notes
                }))
            
            ;; Update mentor's total sessions
            (let ((mentor-profile (unwrap-panic (map-get? mentor-profiles (get mentor session)))))
                (map-set mentor-profiles (get mentor session)
                    (merge mentor-profile {
                        total-sessions: (+ (get total-sessions mentor-profile) u1)
                    })))
            
            (ok true)
        )
    )
)

;; Rate mentor after session
(define-public (rate-mentor 
    (mentor principal)
    (rating uint)
    (feedback (string-ascii 150)))
    
    (let ((existing-rating (map-get? mentor-ratings { mentor: mentor, rater: tx-sender }))
          (mentor-profile (unwrap! (map-get? mentor-profiles mentor) ERR_MENTOR_NOT_FOUND)))
        (begin
            ;; Validate rating range
            (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
            ;; Check if already rated
            (asserts! (is-none existing-rating) ERR_ALREADY_RATED)
            ;; Verify mentorship exists
            (asserts! (is-some (map-get? active-mentorships { mentor: mentor, mentee: tx-sender })) ERR_NOT_AUTHORIZED)
            
            ;; Store rating
            (map-set mentor-ratings { mentor: mentor, rater: tx-sender }
                {
                    rating: rating,
                    feedback: feedback,
                    rated-at: stacks-block-height
                })
            
            ;; Update mentor's average rating (simplified calculation)
            (map-set mentor-profiles mentor
                (merge mentor-profile {
                    avg-rating: rating  ;; Simplified - in production would calculate true average
                }))
            
            (ok true)
        )
    )
)

;; Private function to verify mentor certifications
(define-private (verify-mentor-certifications (cert-ids (list 10 uint)))
    (fold verify-single-cert cert-ids true)
)

;; Helper function to verify single certification
(define-private (verify-single-cert (cert-id uint) (acc bool))
    (and acc
        (match (contract-call? .TechCert get-certification cert-id)
            cert (and 
                (is-eq (get holder cert) tx-sender)
                (is-eq (get status cert) "active"))
            false
        )
    )
)

;; Read-only functions

;; Get mentor profile
(define-read-only (get-mentor-profile (mentor principal))
    (map-get? mentor-profiles mentor)
)

;; Get mentorship request details
(define-read-only (get-mentorship-request (request-id uint))
    (map-get? mentorship-requests request-id)
)

;; Get active mentorship relationship
(define-read-only (get-mentorship (mentor principal) (mentee principal))
    (map-get? active-mentorships { mentor: mentor, mentee: mentee })
)

;; Get session details
(define-read-only (get-session (session-id uint))
    (map-get? mentorship-sessions session-id)
)

;; Get mentor rating from specific rater
(define-read-only (get-mentor-rating (mentor principal) (rater principal))
    (map-get? mentor-ratings { mentor: mentor, rater: rater })
)

;; Get mentor certifications
(define-read-only (get-mentor-certifications (mentor principal))
    (default-to (list) (map-get? mentor-certifications mentor))
)

;; Get system settings
(define-read-only (get-system-settings)
    {
        min-certifications: (var-get min-certifications-required),
        max-mentees-per-mentor: (var-get max-mentees-per-mentor),
        next-request-id: (var-get next-request-id),
        next-session-id: (var-get next-session-id)
    }
)
