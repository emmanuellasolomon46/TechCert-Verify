;; TechCert - IT certification verification platform

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-cert-exists (err u101))
(define-constant err-cert-not-found (err u102))
(define-constant err-max-endorsements-reached (err u103))
(define-constant  err-already-endorsed (err u104))

(define-constant err-service-exists (err u200))
(define-constant err-service-not-found (err u201))
(define-constant err-insufficient-payment (err u202))
(define-constant err-booking-exists (err u203))
(define-constant err-booking-not-found (err u204))
(define-constant err-invalid-service-status (err u205))
(define-constant err-unauthorized-action (err u206))


(define-constant err-escrow-exists (err u300))
(define-constant err-escrow-not-found (err u301))
(define-constant err-insufficient-escrow (err u302))
(define-constant err-escrow-already-released (err u303))
(define-constant err-invalid-escrow-status (err u304))
(define-constant err-escrow-not-disputed (err u305))
(define-constant err-unauthorized-escrow-action (err u306))

(define-data-var next-escrow-id uint u1)

(define-map escrow-agreements
    { escrow-id: uint }
    {
        service-id: uint,
        client: principal,
        provider: principal,
        amount: uint,
        status: (string-ascii 15),
        created-at: uint,
        deadline: uint,
        dispute-reason: (optional (string-ascii 200))
    }
)

(define-map escrow-balances
    { escrow-id: uint }
    { locked-amount: uint }
)

(define-map marketplace-services
    { service-id: uint }
    {
        provider: principal,
        cert-id: uint,
        title: (string-ascii 100),
        description: (string-ascii 500),
        price-per-hour: uint,
        service-type: (string-ascii 20),
        status: (string-ascii 10),
        created-at: uint
    }
)

(define-map service-bookings
    { booking-id: uint }
    {
        service-id: uint,
        client: principal,
        provider: principal,
        hours-booked: uint,
        total-amount: uint,
        booking-status: (string-ascii 15),
        booking-date: uint,
        completion-date: (optional uint)
    }
)

(define-map provider-earnings
    principal
    { total-earned: uint, completed-services: uint }
)

(define-map service-reviews
    { booking-id: uint }
    {
        reviewer: principal,
        rating: uint,
        review-text: (string-ascii 300),
        review-date: uint
    }
)

(define-data-var next-service-id uint u1)
(define-data-var next-booking-id uint u1)
;; Data Maps
(define-map certifications
    { cert-id: uint }
    {
        holder: principal,
        issuer: principal,
        cert-name: (string-ascii 50),
        issue-date: uint,
        expiry-date: uint,
        status: (string-ascii 10)
    }
)

(define-map user-certifications
    principal
    (list 10 uint)
)

;; Public Functions

;; Issue new certification
(define-public (issue-certification (cert-id uint) 
                                  (holder principal)
                                  (cert-name (string-ascii 50))
                                  (expiry-date uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (is-none (get-certification cert-id)) err-cert-exists)
        (map-set certifications
            { cert-id: cert-id }
            {
                holder: holder,
                issuer: tx-sender,
                cert-name: cert-name,
                issue-date: stacks-block-height,
                expiry-date: expiry-date,
                status: "active"
            }
        )
        (ok true)
    )
)

;; Verify certification
(define-read-only (verify-certification (cert-id uint))
    (match (get-certification cert-id)
        cert (ok cert)
        (err err-cert-not-found)
    )
)

;; Get certification details
(define-read-only (get-certification (cert-id uint))
    (map-get? certifications { cert-id: cert-id })
)


;; Add ability to track user skills
(define-map user-skills
    principal
    (list 20 (string-ascii 30))
)

;; Add endorsements tracking
(define-map cert-endorsements
    { cert-id: uint }
    (list 10 principal)
)



;; Get user skills
(define-read-only (get-user-skills (user principal))
    (ok (default-to (list) (map-get? user-skills user)))
)

;; Endorse a certification
(define-public (endorse-certification (cert-id uint))
    (let (
        (current-endorsements (default-to (list) (map-get? cert-endorsements {cert-id: cert-id})))
    )
    (begin
        (asserts! (is-some (get-certification cert-id)) err-cert-not-found)
        (asserts! (< (len current-endorsements) u10) err-max-endorsements-reached)
        (asserts! (not (is-some (index-of current-endorsements tx-sender))) err-already-endorsed)
        (ok (map-set cert-endorsements 
            {cert-id: cert-id} 
            (unwrap-panic (as-max-len? (append current-endorsements tx-sender) u10))))
    ))
)

;; Get certification endorsements
(define-read-only (get-cert-endorsements (cert-id uint))
    (ok (default-to (list) (map-get? cert-endorsements {cert-id: cert-id})))
)


(define-public (revoke-certification (cert-id uint) (reason (string-ascii 100)))
    (let ((cert (get-certification cert-id)))
        (begin 
            (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
            (asserts! (is-some cert) err-cert-not-found)
            (map-set certifications
                { cert-id: cert-id }
                (merge (unwrap-panic cert) { status: "revoked" })
            )
            (ok true)
        )
    )
)

(define-constant err-invalid-transfer (err u105))

(define-public (transfer-certification (cert-id uint) (new-holder principal))
    (let ((cert (get-certification cert-id)))
        (begin
            (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
            (asserts! (is-some cert) err-cert-not-found)
            (asserts! (is-eq (get status (unwrap-panic cert)) "active") err-invalid-transfer)
            (map-set certifications
                { cert-id: cert-id }
                (merge (unwrap-panic cert) { holder: new-holder })
            )
            (ok true)
        )
    )
)


(define-constant err-invalid-renewal (err u106))

(define-public (renew-certification (cert-id uint) (new-expiry uint))
    (let ((cert (get-certification cert-id)))
        (begin
            (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
            (asserts! (is-some cert) err-cert-not-found)
            (asserts! (> new-expiry (get expiry-date (unwrap-panic cert))) err-invalid-renewal)
            (map-set certifications
                { cert-id: cert-id }
                (merge (unwrap-panic cert) { expiry-date: new-expiry })
            )
            (ok true)
        )
    )
)



(define-map user-badges
    principal
    (list 20 (string-ascii 30))
)

(define-public (award-badge (user principal) (badge-name (string-ascii 30)))
    (let ((current-badges (default-to (list) (map-get? user-badges user))))
        (begin
            (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
            (ok (map-set user-badges 
                user 
                (unwrap-panic (as-max-len? (append current-badges badge-name) u20))))
        )
    )
)


(define-map certification-categories
    { cert-id: uint }
    (string-ascii 30)
)

(define-public (set-certification-category (cert-id uint) (category (string-ascii 30)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (is-some (get-certification cert-id)) err-cert-not-found)
        (ok (map-set certification-categories { cert-id: cert-id } category))
    )
)



(define-map user-experience
    principal
    uint
)

(define-public (add-experience-points (user principal) (points uint))
    (let ((current-points (default-to u0 (map-get? user-experience user))))
        (begin
            (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
            (ok (map-set user-experience user (+ current-points points)))
        )
    )
)


(define-map certification-prerequisites
    { cert-id: uint }
    (list 5 uint)
)

(define-constant err-prerequisites-not-met (err u107))

(define-public (set-prerequisites (cert-id uint) (required-certs (list 5 uint)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (ok (map-set certification-prerequisites { cert-id: cert-id } required-certs))
    )
)


(define-map certification-comments
    { cert-id: uint }
    (list 10 {commenter: principal, comment: (string-ascii 200)})
)

(define-public (add-certification-comment (cert-id uint) (comment (string-ascii 200)))
    (let ((current-comments (default-to (list) (map-get? certification-comments {cert-id: cert-id}))))
        (begin
            (asserts! (is-some (get-certification cert-id)) err-cert-not-found)
            (ok (map-set certification-comments 
                {cert-id: cert-id} 
                (unwrap-panic (as-max-len? 
                    (append current-comments {commenter: tx-sender, comment: comment}) 
                    u10))))
        )
    )
)



;; Define constants
(define-constant err-invalid-rating (err u108))
(define-constant err-already-rated (err u109))

;; Define rating map
(define-map certification-ratings
    { cert-id: uint }
    { total-score: uint, num-ratings: uint }
)

;; Define user ratings map to prevent multiple ratings
(define-map user-ratings
    { cert-id: uint, rater: principal }
    bool
)

;; Add rating function
(define-public (rate-certification (cert-id uint) (score uint))
    (let (
        (current-rating (default-to { total-score: u0, num-ratings: u0 } 
                        (map-get? certification-ratings { cert-id: cert-id })))
        (has-rated (default-to false 
                   (map-get? user-ratings { cert-id: cert-id, rater: tx-sender })))
    )
    (begin
        (asserts! (is-some (get-certification cert-id)) err-cert-not-found)
        (asserts! (and (>= score u1) (<= score u5)) err-invalid-rating)
        (asserts! (not has-rated) err-already-rated)
        (map-set user-ratings { cert-id: cert-id, rater: tx-sender } true)
        (ok (map-set certification-ratings
            { cert-id: cert-id }
            {
                total-score: (+ (get total-score current-rating) score),
                num-ratings: (+ (get num-ratings current-rating) u1)
            }))
    ))
)


;; Define verification history map
(define-map verification-history
    { cert-id: uint }
    (list 50 { verifier: principal, timestamp: uint })
)

;; Add verification tracking
(define-public (track-verification (cert-id uint))
    (let (
        (current-history (default-to (list) 
                         (map-get? verification-history { cert-id: cert-id })))
    )
    (begin
        (asserts! (is-some (get-certification cert-id)) err-cert-not-found)
        (ok (map-set verification-history
            { cert-id: cert-id }
            (unwrap-panic (as-max-len? 
                (append current-history { verifier: tx-sender, timestamp: stacks-block-height })
                u50))))
    ))
)


;; Define milestone map
(define-map certification-milestones
    principal
    { certs-count: uint, last-milestone: uint }
)

(define-constant milestone-levels (list u5 u10 u20 u50))

;; Update milestones when certification is issued
(define-public (check-milestones (user principal))
    (let (
        (user-certs (default-to (list) (map-get? user-certifications user)))
        (current-milestones (default-to { certs-count: u0, last-milestone: u0 }
                            (map-get? certification-milestones user)))
    )
    (begin
        (map-set certification-milestones
            user
            {
                certs-count: (len user-certs),
                last-milestone: (len user-certs)
            })
        (ok true)
    ))
)


;; Define achievement tracking map
(define-map time-achievements
    principal
    { first-cert-date: uint, achievement-level: uint }
)

;; Track time-based achievements
(define-public (update-time-achievements (user principal))
    (let (
        (current-achievements (default-to { first-cert-date: stacks-block-height, achievement-level: u0 }
                             (map-get? time-achievements user)))
    )
    (begin
        (map-set time-achievements
            user
            {
                first-cert-date: (get first-cert-date current-achievements),
                achievement-level: (+ (get achievement-level current-achievements) u1)
            })
        (ok true)
    ))
)

;; Define specialization map
(define-map user-specializations
    principal
    { primary: (string-ascii 30), secondary: (string-ascii 30) }
)

;; Set user specialization
(define-public (set-specialization (primary (string-ascii 30)) (secondary (string-ascii 30)))
    (ok (map-set user-specializations
        tx-sender
        { primary: primary, secondary: secondary }))
)

;; Define constants
(define-constant err-invalid-challenge (err u110))

;; Define challenge map
(define-map certification-challenges
    { cert-id: uint }
    { challenger: principal, reason: (string-ascii 200), status: (string-ascii 20) }
)

;; Submit certification challenge
(define-public (challenge-certification (cert-id uint) (reason (string-ascii 200)))
    (begin
        (asserts! (is-some (get-certification cert-id)) err-cert-not-found)
        (asserts! (is-none (map-get? certification-challenges { cert-id: cert-id })) 
                 err-invalid-challenge)
        (ok (map-set certification-challenges
            { cert-id: cert-id }
            { challenger: tx-sender, reason: reason, status: "pending" }))
    )
)


;; Define notification map
(define-map expiry-notifications
    { cert-id: uint }
    { notified: bool, notification-date: uint }
)

;; Check and set notification status
(define-public (check-expiry-notification (cert-id uint))
    (let (
        (cert (get-certification cert-id))
        (current-notification (default-to { notified: false, notification-date: u0 }
                             (map-get? expiry-notifications { cert-id: cert-id })))
    )
    (begin
        (asserts! (is-some cert) err-cert-not-found)
        (ok (map-set expiry-notifications
            { cert-id: cert-id }
            {
                notified: true,
                notification-date: stacks-block-height
            }))
    ))
)


(define-constant err-invalid-batch (err u111))
(define-constant max-batch-size u10)

(define-public (batch-issue-certifications 
    (cert-ids (list 10 uint))
    (holders (list 10 principal))
    (cert-names (list 10 (string-ascii 50)))
    (expiry-dates (list 10 uint)))
    
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (> (len cert-ids) u0) err-invalid-batch)
        (asserts! (is-eq (len cert-ids) (len holders)) err-invalid-batch)
        (asserts! (is-eq (len cert-ids) (len cert-names)) err-invalid-batch)
        (asserts! (is-eq (len cert-ids) (len expiry-dates)) err-invalid-batch)
        
        (ok (map issue-certification-internal 
            cert-ids
            holders 
            cert-names
            expiry-dates))
    )
)

(define-private (issue-certification-internal 
    (cert-id uint)
    (holder principal)
    (cert-name (string-ascii 50))
    (expiry-date uint))
    
    (map-set certifications
        { cert-id: cert-id }
        {
            holder: holder,
            issuer: tx-sender,
            cert-name: cert-name,
            issue-date: stacks-block-height,
            expiry-date: expiry-date,
            status: "active"
        }
    )
)


(define-constant err-template-exists (err u112))
(define-constant err-template-not-found (err u113))

(define-map certification-templates
    { template-id: uint }
    {
        name: (string-ascii 50),
        validity-period: uint,
        category: (string-ascii 30),
        required-prerequisites: (list 5 uint)
    }
)

(define-public (create-certification-template
    (template-id uint)
    (name (string-ascii 50))
    (validity-period uint)
    (category (string-ascii 30))
    (prerequisites (list 5 uint)))
    
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (is-none (map-get? certification-templates {template-id: template-id})) err-template-exists)
        
        (ok (map-set certification-templates
            {template-id: template-id}
            {
                name: name,
                validity-period: validity-period,
                category: category,
                required-prerequisites: prerequisites
            }))
    )
)

(define-public (issue-from-template 
    (template-id uint)
    (cert-id uint)
    (holder principal))
    
    (let ((template (unwrap! (map-get? certification-templates {template-id: template-id}) err-template-not-found)))
        (begin
            (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
            (issue-certification cert-id 
                               holder 
                               (get name template)
                               (+ stacks-block-height (get validity-period template)))
        )
    )
)


(define-public (create-marketplace-service
    (cert-id uint)
    (title (string-ascii 100))
    (description (string-ascii 500))
    (price-per-hour uint)
    (service-type (string-ascii 20)))
    
    (let ((service-id (var-get next-service-id))
          (cert (get-certification cert-id)))
        (begin
            (asserts! (is-some cert) err-cert-not-found)
            (asserts! (is-eq (get holder (unwrap-panic cert)) tx-sender) err-not-authorized)
            (asserts! (is-eq (get status (unwrap-panic cert)) "active") err-invalid-service-status)
            (asserts! (is-none (map-get? marketplace-services {service-id: service-id})) err-service-exists)
            
            (map-set marketplace-services
                {service-id: service-id}
                {
                    provider: tx-sender,
                    cert-id: cert-id,
                    title: title,
                    description: description,
                    price-per-hour: price-per-hour,
                    service-type: service-type,
                    status: "active",
                    created-at: stacks-block-height
                })
            
            (var-set next-service-id (+ service-id u1))
            (ok service-id)
        )
    )
)


(define-public (create-escrow
    (service-id uint)
    (provider principal)
    (amount uint)
    (deadline uint))
    
    (let ((escrow-id (var-get next-escrow-id))
          (service (unwrap! (map-get? marketplace-services {service-id: service-id}) err-service-not-found)))
        (begin
            (asserts! (is-eq (get provider service) provider) err-unauthorized-action)
            (asserts! (> amount u0) err-insufficient-payment)
            (asserts! (> deadline stacks-block-height) err-invalid-service-status)
            
            (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
            
            (map-set escrow-agreements
                {escrow-id: escrow-id}
                {
                    service-id: service-id,
                    client: tx-sender,
                    provider: provider,
                    amount: amount,
                    status: "active",
                    created-at: stacks-block-height,
                    deadline: deadline,
                    dispute-reason: none
                })
            
            (map-set escrow-balances
                {escrow-id: escrow-id}
                {locked-amount: amount})
            
            (var-set next-escrow-id (+ escrow-id u1))
            (ok escrow-id)
        )
    )
)

(define-public (release-escrow (escrow-id uint))
    (let ((escrow (unwrap! (map-get? escrow-agreements {escrow-id: escrow-id}) err-escrow-not-found))
          (balance (unwrap! (map-get? escrow-balances {escrow-id: escrow-id}) err-escrow-not-found)))
        (begin
            (asserts! (is-eq tx-sender (get client escrow)) err-unauthorized-escrow-action)
            (asserts! (is-eq (get status escrow) "active") err-invalid-escrow-status)
            
            (try! (as-contract (stx-transfer? (get locked-amount balance) tx-sender (get provider escrow))))
            
            (map-set escrow-agreements
                {escrow-id: escrow-id}
                (merge escrow {status: "released"}))
            
            (map-set escrow-balances
                {escrow-id: escrow-id}
                {locked-amount: u0})
            
            (ok true)
        )
    )
)

(define-public (dispute-escrow (escrow-id uint) (reason (string-ascii 200)))
    (let ((escrow (unwrap! (map-get? escrow-agreements {escrow-id: escrow-id}) err-escrow-not-found)))
        (begin
            (asserts! (or (is-eq tx-sender (get client escrow)) (is-eq tx-sender (get provider escrow))) err-unauthorized-escrow-action)
            (asserts! (is-eq (get status escrow) "active") err-invalid-escrow-status)
            
            (map-set escrow-agreements
                {escrow-id: escrow-id}
                (merge escrow {
                    status: "disputed",
                    dispute-reason: (some reason)
                }))
            
            (ok true)
        )
    )
)

(define-public (resolve-dispute (escrow-id uint) (release-to-provider bool))
    (let ((escrow (unwrap! (map-get? escrow-agreements {escrow-id: escrow-id}) err-escrow-not-found))
          (balance (unwrap! (map-get? escrow-balances {escrow-id: escrow-id}) err-escrow-not-found)))
        (begin
            (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
            (asserts! (is-eq (get status escrow) "disputed") err-escrow-not-disputed)
            
            (let ((recipient (if release-to-provider (get provider escrow) (get client escrow))))
                (try! (as-contract (stx-transfer? (get locked-amount balance) tx-sender recipient)))
                
                (map-set escrow-agreements
                    {escrow-id: escrow-id}
                    (merge escrow {status: "resolved"}))
                
                (map-set escrow-balances
                    {escrow-id: escrow-id}
                    {locked-amount: u0})
                
                (ok true)
            )
        )
    )
)

(define-public (refund-expired-escrow (escrow-id uint))
    (let ((escrow (unwrap! (map-get? escrow-agreements {escrow-id: escrow-id}) err-escrow-not-found))
          (balance (unwrap! (map-get? escrow-balances {escrow-id: escrow-id}) err-escrow-not-found)))
        (begin
            (asserts! (is-eq (get status escrow) "active") err-invalid-escrow-status)
            (asserts! (> stacks-block-height (get deadline escrow)) err-invalid-escrow-status)
            
            (try! (as-contract (stx-transfer? (get locked-amount balance) tx-sender (get client escrow))))
            
            (map-set escrow-agreements
                {escrow-id: escrow-id}
                (merge escrow {status: "refunded"}))
            
            (map-set escrow-balances
                {escrow-id: escrow-id}
                {locked-amount: u0})
            
            (ok true)
        )
    )
)

(define-read-only (get-escrow-agreement (escrow-id uint))
    (map-get? escrow-agreements {escrow-id: escrow-id})
)

(define-read-only (get-escrow-balance (escrow-id uint))
    (map-get? escrow-balances {escrow-id: escrow-id})
)

(define-read-only (get-next-escrow-id)
    (var-get next-escrow-id)
)

(define-public (book-service
    (service-id uint)
    (hours-requested uint))
    
    (let ((service (unwrap! (map-get? marketplace-services {service-id: service-id}) err-service-not-found))
          (booking-id (var-get next-booking-id))
          (total-cost (* (get price-per-hour service) hours-requested)))
        (begin
            (asserts! (is-eq (get status service) "active") err-invalid-service-status)
            (asserts! (> hours-requested u0) err-invalid-service-status)
            (asserts! (not (is-eq tx-sender (get provider service))) err-unauthorized-action)
            
            (try! (stx-transfer? total-cost tx-sender (get provider service)))
            
            (map-set service-bookings
                {booking-id: booking-id}
                {
                    service-id: service-id,
                    client: tx-sender,
                    provider: (get provider service),
                    hours-booked: hours-requested,
                    total-amount: total-cost,
                    booking-status: "confirmed",
                    booking-date: stacks-block-height,
                    completion-date: none
                })
            
            (var-set next-booking-id (+ booking-id u1))
            (ok booking-id)
        )
    )
)

(define-public (complete-service (booking-id uint))
    (let ((booking (unwrap! (map-get? service-bookings {booking-id: booking-id}) err-booking-not-found)))
        (begin
            (asserts! (is-eq tx-sender (get provider booking)) err-unauthorized-action)
            (asserts! (is-eq (get booking-status booking) "confirmed") err-invalid-service-status)
            
            (map-set service-bookings
                {booking-id: booking-id}
                (merge booking {
                    booking-status: "completed",
                    completion-date: (some stacks-block-height)
                }))
            
            (let ((current-earnings (default-to {total-earned: u0, completed-services: u0}
                                               (map-get? provider-earnings tx-sender))))
                (map-set provider-earnings
                    tx-sender
                    {
                        total-earned: (+ (get total-earned current-earnings) (get total-amount booking)),
                        completed-services: (+ (get completed-services current-earnings) u1)
                    }))
            
            (ok true)
        )
    )
)

(define-public (review-service
    (booking-id uint)
    (rating uint)
    (review-text (string-ascii 300)))
    
    (let ((booking (unwrap! (map-get? service-bookings {booking-id: booking-id}) err-booking-not-found)))
        (begin
            (asserts! (is-eq tx-sender (get client booking)) err-unauthorized-action)
            (asserts! (is-eq (get booking-status booking) "completed") err-invalid-service-status)
            (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
            (asserts! (is-none (map-get? service-reviews {booking-id: booking-id})) err-already-rated)
            
            (map-set service-reviews
                {booking-id: booking-id}
                {
                    reviewer: tx-sender,
                    rating: rating,
                    review-text: review-text,
                    review-date: stacks-block-height
                })
            
            (ok true)
        )
    )
)

(define-public (update-service-status (service-id uint) (new-status (string-ascii 10)))
    (let ((service (unwrap! (map-get? marketplace-services {service-id: service-id}) err-service-not-found)))
        (begin
            (asserts! (is-eq tx-sender (get provider service)) err-unauthorized-action)
            
            (map-set marketplace-services
                {service-id: service-id}
                (merge service {status: new-status}))
            
            (ok true)
        )
    )
)

(define-read-only (get-marketplace-service (service-id uint))
    (map-get? marketplace-services {service-id: service-id})
)

(define-read-only (get-service-booking (booking-id uint))
    (map-get? service-bookings {booking-id: booking-id})
)

(define-read-only (get-provider-earnings (provider principal))
    (default-to {total-earned: u0, completed-services: u0}
                (map-get? provider-earnings provider))
)

(define-read-only (get-service-review (booking-id uint))
    (map-get? service-reviews {booking-id: booking-id})
)

(define-read-only (get-next-service-id)
    (var-get next-service-id)
)

(define-read-only (get-next-booking-id)
    (var-get next-booking-id)
)