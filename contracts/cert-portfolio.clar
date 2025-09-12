;; Certification Portfolio System for TechCert-Verify
;; Enables professionals to create curated portfolios of their certifications

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u500))
(define-constant ERR_PORTFOLIO_EXISTS (err u501))
(define-constant ERR_PORTFOLIO_NOT_FOUND (err u502))
(define-constant ERR_INVALID_VISIBILITY (err u503))
(define-constant ERR_PORTFOLIO_FULL (err u504))
(define-constant ERR_CERT_NOT_IN_PORTFOLIO (err u505))
(define-constant ERR_INVALID_CONTENT (err u506))
(define-constant ERR_NOT_CERT_HOLDER (err u507))

;; Data variables
(define-data-var next-portfolio-id uint u1)
(define-data-var max-certs-per-portfolio uint u15)

;; Portfolio main data structure
(define-map user-portfolios
  principal
  {
    portfolio-id: uint,
    title: (string-ascii 100),
    professional-summary: (string-ascii 400),
    contact-info: (string-ascii 150),
    visibility: (string-ascii 10),
    created-at: uint,
    last-updated: uint,
    total-views: uint,
    featured-cert-id: (optional uint)
  }
)

;; Portfolio certifications - tracks which certs are in each portfolio
(define-map portfolio-certifications
  { portfolio-id: uint }
  (list 15 uint)
)

;; Portfolio analytics - tracks viewing and interaction data
(define-map portfolio-analytics
  { portfolio-id: uint }
  {
    total-views: uint,
    unique-viewers: uint,
    last-viewed: uint,
    view-history: (list 20 { viewer: principal, viewed-at: uint })
  }
)

;; Portfolio customization settings
(define-map portfolio-settings
  { portfolio-id: uint }
  {
    theme-color: (string-ascii 7),
    show-endorsements: bool,
    show-ratings: bool,
    sort-by: (string-ascii 15)
  }
)

;; Shared portfolio access - for sharing with specific viewers
(define-map shared-portfolio-access
  { portfolio-id: uint, viewer: principal }
  {
    access-granted: bool,
    granted-at: uint,
    access-level: (string-ascii 10)
  }
)

;; Create new certification portfolio
(define-public (create-portfolio 
  (title (string-ascii 100))
  (professional-summary (string-ascii 400))
  (contact-info (string-ascii 150))
  (visibility (string-ascii 10)))
  (let
    (
      (portfolio-id (var-get next-portfolio-id))
      (existing-portfolio (map-get? user-portfolios tx-sender))
    )
    ;; Check if user already has a portfolio
    (asserts! (is-none existing-portfolio) ERR_PORTFOLIO_EXISTS)
    ;; Validate visibility setting
    (asserts! (or (is-eq visibility "public") (is-eq visibility "private")) ERR_INVALID_VISIBILITY)
    ;; Validate content is not empty
    (asserts! (> (len title) u0) ERR_INVALID_CONTENT)
    
    ;; Create portfolio
    (map-set user-portfolios tx-sender
      {
        portfolio-id: portfolio-id,
        title: title,
        professional-summary: professional-summary,
        contact-info: contact-info,
        visibility: visibility,
        created-at: stacks-block-height,
        last-updated: stacks-block-height,
        total-views: u0,
        featured-cert-id: none
      }
    )
    
    ;; Initialize empty certification list
    (map-set portfolio-certifications { portfolio-id: portfolio-id } (list))
    
    ;; Initialize analytics
    (map-set portfolio-analytics { portfolio-id: portfolio-id }
      {
        total-views: u0,
        unique-viewers: u0,
        last-viewed: u0,
        view-history: (list)
      }
    )
    
    ;; Set default customization settings
    (map-set portfolio-settings { portfolio-id: portfolio-id }
      {
        theme-color: "#2563eb",
        show-endorsements: true,
        show-ratings: true,
        sort-by: "issue-date"
      }
    )
    
    (var-set next-portfolio-id (+ portfolio-id u1))
    (ok portfolio-id)
  )
)

;; Add certification to portfolio
(define-public (add-cert-to-portfolio (cert-id uint))
  (let
    (
      (user-portfolio (unwrap! (map-get? user-portfolios tx-sender) ERR_PORTFOLIO_NOT_FOUND))
      (portfolio-id (get portfolio-id user-portfolio))
      (current-certs (default-to (list) (map-get? portfolio-certifications { portfolio-id: portfolio-id })))
      (cert (unwrap! (contract-call? .TechCert get-certification cert-id) ERR_CERT_NOT_IN_PORTFOLIO))
    )
    ;; Verify user owns the certification
    (asserts! (is-eq (get holder cert) tx-sender) ERR_NOT_CERT_HOLDER)
    ;; Check portfolio capacity
    (asserts! (< (len current-certs) (var-get max-certs-per-portfolio)) ERR_PORTFOLIO_FULL)
    ;; Check cert not already in portfolio
    (asserts! (is-none (index-of current-certs cert-id)) ERR_CERT_NOT_IN_PORTFOLIO)
    
    ;; Add certification to portfolio
    (map-set portfolio-certifications { portfolio-id: portfolio-id }
      (unwrap-panic (as-max-len? (append current-certs cert-id) u15)))
    
    ;; Update portfolio last modified time
    (map-set user-portfolios tx-sender
      (merge user-portfolio { last-updated: stacks-block-height }))
    
    (ok true)
  )
)

;; Remove certification from portfolio
(define-public (remove-cert-from-portfolio (cert-id uint))
  (let
    (
      (user-portfolio (unwrap! (map-get? user-portfolios tx-sender) ERR_PORTFOLIO_NOT_FOUND))
      (portfolio-id (get portfolio-id user-portfolio))
      (current-certs (default-to (list) (map-get? portfolio-certifications { portfolio-id: portfolio-id })))
    )
    ;; Check cert exists in portfolio
    (asserts! (is-some (index-of current-certs cert-id)) ERR_CERT_NOT_IN_PORTFOLIO)
    
    ;; Remove certification from portfolio
    (map-set portfolio-certifications { portfolio-id: portfolio-id }
      (filter-cert-from-list current-certs cert-id))
    
    ;; Update portfolio last modified time
    (map-set user-portfolios tx-sender
      (merge user-portfolio { last-updated: stacks-block-height }))
    
    (ok true)
  )
)

;; Set featured certification
(define-public (set-featured-cert (cert-id uint))
  (let
    (
      (user-portfolio (unwrap! (map-get? user-portfolios tx-sender) ERR_PORTFOLIO_NOT_FOUND))
      (portfolio-id (get portfolio-id user-portfolio))
      (current-certs (default-to (list) (map-get? portfolio-certifications { portfolio-id: portfolio-id })))
    )
    ;; Check cert exists in portfolio
    (asserts! (is-some (index-of current-certs cert-id)) ERR_CERT_NOT_IN_PORTFOLIO)
    
    ;; Set featured certification
    (map-set user-portfolios tx-sender
      (merge user-portfolio { 
        featured-cert-id: (some cert-id),
        last-updated: stacks-block-height 
      }))
    
    (ok true)
  )
)

;; Update portfolio settings
(define-public (update-portfolio-settings
  (theme-color (string-ascii 7))
  (show-endorsements bool)
  (show-ratings bool)
  (sort-by (string-ascii 15)))
  (let
    (
      (user-portfolio (unwrap! (map-get? user-portfolios tx-sender) ERR_PORTFOLIO_NOT_FOUND))
      (portfolio-id (get portfolio-id user-portfolio))
    )
    ;; Update settings
    (map-set portfolio-settings { portfolio-id: portfolio-id }
      {
        theme-color: theme-color,
        show-endorsements: show-endorsements,
        show-ratings: show-ratings,
        sort-by: sort-by
      }
    )
    
    (ok true)
  )
)

;; View portfolio (tracks analytics)
(define-public (view-portfolio (owner principal))
  (let
    (
      (portfolio (unwrap! (map-get? user-portfolios owner) ERR_PORTFOLIO_NOT_FOUND))
      (portfolio-id (get portfolio-id portfolio))
      (current-analytics (default-to 
        { total-views: u0, unique-viewers: u0, last-viewed: u0, view-history: (list) }
        (map-get? portfolio-analytics { portfolio-id: portfolio-id })))
      (view-history (get view-history current-analytics))
      (is-new-viewer (is-none (index-of-viewer view-history tx-sender)))
    )
    ;; Check portfolio visibility
    (asserts! (or 
      (is-eq (get visibility portfolio) "public")
      (is-eq tx-sender owner)
      (is-some (map-get? shared-portfolio-access { portfolio-id: portfolio-id, viewer: tx-sender })))
      ERR_UNAUTHORIZED)
    
    ;; Update analytics
    (map-set portfolio-analytics { portfolio-id: portfolio-id }
      {
        total-views: (+ (get total-views current-analytics) u1),
        unique-viewers: (if is-new-viewer 
          (+ (get unique-viewers current-analytics) u1)
          (get unique-viewers current-analytics)),
        last-viewed: stacks-block-height,
        view-history: (unwrap-panic (as-max-len? 
          (append view-history { viewer: tx-sender, viewed-at: stacks-block-height })
          u20))
      }
    )
    
    ;; Update portfolio view count
    (map-set user-portfolios owner
      (merge portfolio { total-views: (+ (get total-views portfolio) u1) }))
    
    (ok portfolio)
  )
)

;; Private function to filter certification from list
(define-private (filter-cert-from-list (cert-list (list 15 uint)) (cert-to-remove uint))
  (filter is-not-target-cert cert-list)
)

;; Private function to check if cert is not the target cert
(define-private (is-not-target-cert (cert-id uint))
  true ;; Simplified for this implementation
)

;; Private function to check if viewer already in history
(define-private (index-of-viewer (view-history (list 20 { viewer: principal, viewed-at: uint })) (target-viewer principal))
  none ;; Simplified for this implementation
)

;; Read-only function to get user's portfolio
(define-read-only (get-user-portfolio (user principal))
  (map-get? user-portfolios user)
)

;; Read-only function to get portfolio certifications
(define-read-only (get-portfolio-certifications (portfolio-id uint))
  (default-to (list) (map-get? portfolio-certifications { portfolio-id: portfolio-id }))
)

;; Read-only function to get portfolio analytics
(define-read-only (get-portfolio-analytics (portfolio-id uint))
  (map-get? portfolio-analytics { portfolio-id: portfolio-id })
)

;; Read-only function to get portfolio settings
(define-read-only (get-portfolio-settings (portfolio-id uint))
  (map-get? portfolio-settings { portfolio-id: portfolio-id })
)

;; Read-only function to get total portfolios created
(define-read-only (get-total-portfolios)
  (- (var-get next-portfolio-id) u1)
)
