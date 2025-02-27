;; TechCert - IT certification verification platform

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-cert-exists (err u101))
(define-constant err-cert-not-found (err u102))
(define-constant err-max-endorsements-reached (err u103))
(define-constant  err-already-endorsed (err u104))

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
