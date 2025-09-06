;; File Sharing Expiry System for FileTag
;; Enables time-limited file sharing with rental functionality

(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-FILE-NOT-FOUND (err u201))
(define-constant ERR-SHARE-NOT-FOUND (err u202))
(define-constant ERR-SHARE-EXPIRED (err u203))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u204))
(define-constant ERR-INVALID-DURATION (err u205))
(define-constant ERR-SHARE-ALREADY-EXISTS (err u206))
(define-constant ERR-INVALID-PRICE (err u207))

(define-data-var next-share-id uint u1)
(define-data-var platform-fee-percentage uint u5) ;; 5% platform fee
(define-data-var contract-owner principal tx-sender)

;; Track file sharing sessions with expiry
(define-map file-shares
  { share-id: uint }
  {
    file-id: uint,
    owner: principal,
    renter: principal,
    rental-price: uint,
    start-time: uint,
    expiry-time: uint,
    access-count: uint,
    max-access-count: (optional uint),
    is-active: bool,
    auto-extend: bool,
    payment-received: uint
  }
)

;; Map file and renter to active share
(define-map active-file-shares
  { file-id: uint, renter: principal }
  { share-id: uint, expiry-time: uint }
)

;; Track rental earnings for file owners
(define-map owner-earnings
  { owner: principal }
  { total-earned: uint, active-rentals: uint, total-rentals: uint }
)

;; File rental settings set by owners
(define-map file-rental-settings
  { file-id: uint }
  {
    hourly-rate: uint,
    daily-rate: uint,
    weekly-rate: uint,
    max-concurrent-rentals: uint,
    min-rental-duration: uint,
    max-rental-duration: uint,
    requires-approval: bool,
    active-rental-count: uint
  }
)

;; Usage analytics for shared files
(define-map file-usage-stats
  { file-id: uint }
  { total-shares: uint, total-revenue: uint, last-shared: uint, top-renter: (optional principal) }
)

;; Create time-limited file share
(define-public (create-file-share
  (file-id uint)
  (renter principal)
  (duration-hours uint)
  (max-accesses (optional uint))
  (auto-extend bool))
  (let (
    (share-id (var-get next-share-id))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    (expiry-time (+ current-time (* duration-hours u3600)))
    (rental-settings (map-get? file-rental-settings { file-id: file-id }))
    (rental-rate (match rental-settings
      settings (get hourly-rate settings)
      u10)) ;; Default 10 STX per hour
    (total-cost (* duration-hours rental-rate))
    (platform-fee (/ (* total-cost (var-get platform-fee-percentage)) u100))
    (owner-payment (- total-cost platform-fee))
  )
    ;; Validate inputs
    (asserts! (> duration-hours u0) ERR-INVALID-DURATION)
    (asserts! (<= duration-hours u168) ERR-INVALID-DURATION) ;; Max 1 week
    (asserts! (> total-cost u0) ERR-INVALID-PRICE)
    
    ;; Check if share already exists for this file and renter
    (asserts! (is-none (map-get? active-file-shares { file-id: file-id, renter: renter })) ERR-SHARE-ALREADY-EXISTS)
    
    ;; Payment validation (simplified - in real implementation would transfer STX)
    (asserts! (>= total-cost u1) ERR-INSUFFICIENT-PAYMENT)
    
    ;; Create the share
    (map-set file-shares
      { share-id: share-id }
      {
        file-id: file-id,
        owner: tx-sender,
        renter: renter,
        rental-price: total-cost,
        start-time: current-time,
        expiry-time: expiry-time,
        access-count: u0,
        max-access-count: max-accesses,
        is-active: true,
        auto-extend: auto-extend,
        payment-received: total-cost
      }
    )
    
    ;; Track active share
    (map-set active-file-shares
      { file-id: file-id, renter: renter }
      { share-id: share-id, expiry-time: expiry-time }
    )
    
    ;; Update owner earnings
    (let (
      (current-earnings (default-to { total-earned: u0, active-rentals: u0, total-rentals: u0 }
        (map-get? owner-earnings { owner: tx-sender })))
    )
      (map-set owner-earnings
        { owner: tx-sender }
        {
          total-earned: (+ (get total-earned current-earnings) owner-payment),
          active-rentals: (+ (get active-rentals current-earnings) u1),
          total-rentals: (+ (get total-rentals current-earnings) u1)
        }
      )
    )
    
    ;; Update file usage stats
    (let (
      (current-stats (default-to { total-shares: u0, total-revenue: u0, last-shared: u0, top-renter: none }
        (map-get? file-usage-stats { file-id: file-id })))
    )
      (map-set file-usage-stats
        { file-id: file-id }
        {
          total-shares: (+ (get total-shares current-stats) u1),
          total-revenue: (+ (get total-revenue current-stats) total-cost),
          last-shared: current-time,
          top-renter: (some renter)
        }
      )
    )
    
    (var-set next-share-id (+ share-id u1))
    (ok share-id)
  )
)

;; Access shared file (checks expiry and increments usage)
(define-public (access-shared-file (share-id uint))
  (let (
    (share-data (unwrap! (map-get? file-shares { share-id: share-id }) ERR-SHARE-NOT-FOUND))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
  )
    ;; Check authorization
    (asserts! (or (is-eq tx-sender (get renter share-data)) (is-eq tx-sender (get owner share-data))) ERR-NOT-AUTHORIZED)
    
    ;; Check if share is active
    (asserts! (get is-active share-data) ERR-SHARE-EXPIRED)
    
    ;; Check if not expired
    (asserts! (<= current-time (get expiry-time share-data)) ERR-SHARE-EXPIRED)
    
    ;; Check access count limit
    (match (get max-access-count share-data)
      max-count (asserts! (< (get access-count share-data) max-count) ERR-SHARE-EXPIRED)
      true
    )
    
    ;; Update access count
    (map-set file-shares
      { share-id: share-id }
      (merge share-data { access-count: (+ (get access-count share-data) u1) })
    )
    
    (ok true)
  )
)

;; Extend share duration (with additional payment)
(define-public (extend-share-duration (share-id uint) (additional-hours uint))
  (let (
    (share-data (unwrap! (map-get? file-shares { share-id: share-id }) ERR-SHARE-NOT-FOUND))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    (rental-settings (map-get? file-rental-settings { file-id: (get file-id share-data) }))
    (rental-rate (match rental-settings
      settings (get hourly-rate settings)
      u10))
    (extension-cost (* additional-hours rental-rate))
    (new-expiry-time (+ (get expiry-time share-data) (* additional-hours u3600)))
  )
    ;; Check authorization
    (asserts! (is-eq tx-sender (get renter share-data)) ERR-NOT-AUTHORIZED)
    
    ;; Check if share is still active
    (asserts! (get is-active share-data) ERR-SHARE-EXPIRED)
    
    ;; Validate extension
    (asserts! (> additional-hours u0) ERR-INVALID-DURATION)
    (asserts! (>= extension-cost u1) ERR-INSUFFICIENT-PAYMENT)
    
    ;; Update share
    (map-set file-shares
      { share-id: share-id }
      (merge share-data {
        expiry-time: new-expiry-time,
        payment-received: (+ (get payment-received share-data) extension-cost)
      })
    )
    
    ;; Update active share mapping
    (map-set active-file-shares
      { file-id: (get file-id share-data), renter: (get renter share-data) }
      { share-id: share-id, expiry-time: new-expiry-time }
    )
    
    (ok new-expiry-time)
  )
)

;; Revoke active share early
(define-public (revoke-file-share (share-id uint))
  (let (
    (share-data (unwrap! (map-get? file-shares { share-id: share-id }) ERR-SHARE-NOT-FOUND))
  )
    ;; Check authorization (owner or renter can revoke)
    (asserts! (or (is-eq tx-sender (get owner share-data)) (is-eq tx-sender (get renter share-data))) ERR-NOT-AUTHORIZED)
    
    ;; Deactivate share
    (map-set file-shares
      { share-id: share-id }
      (merge share-data { is-active: false })
    )
    
    ;; Remove from active shares
    (map-delete active-file-shares { file-id: (get file-id share-data), renter: (get renter share-data) })
    
    ;; Update owner earnings (reduce active count)
    (let (
      (current-earnings (default-to { total-earned: u0, active-rentals: u0, total-rentals: u0 }
        (map-get? owner-earnings { owner: (get owner share-data) })))
    )
      (map-set owner-earnings
        { owner: (get owner share-data) }
        (merge current-earnings { active-rentals: (- (get active-rentals current-earnings) u1) })
      )
    )
    
    (ok true)
  )
)

;; Set rental settings for a file
(define-public (set-file-rental-settings
  (file-id uint)
  (hourly-rate uint)
  (daily-rate uint)
  (weekly-rate uint)
  (max-concurrent uint)
  (min-duration uint)
  (max-duration uint)
  (requires-approval bool))
  (begin
    ;; Basic validation
    (asserts! (> hourly-rate u0) ERR-INVALID-PRICE)
    (asserts! (> daily-rate u0) ERR-INVALID-PRICE)
    (asserts! (> weekly-rate u0) ERR-INVALID-PRICE)
    (asserts! (<= min-duration max-duration) ERR-INVALID-DURATION)
    
    (map-set file-rental-settings
      { file-id: file-id }
      {
        hourly-rate: hourly-rate,
        daily-rate: daily-rate,
        weekly-rate: weekly-rate,
        max-concurrent-rentals: max-concurrent,
        min-rental-duration: min-duration,
        max-rental-duration: max-duration,
        requires-approval: requires-approval,
        active-rental-count: u0
      }
    )
    (ok true)
  )
)

;; Cleanup expired shares (can be called by anyone)
(define-public (cleanup-expired-share (share-id uint))
  (let (
    (share-data (unwrap! (map-get? file-shares { share-id: share-id }) ERR-SHARE-NOT-FOUND))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
  )
    ;; Check if expired
    (asserts! (> current-time (get expiry-time share-data)) ERR-SHARE-EXPIRED)
    
    ;; Only cleanup if still marked as active
    (if (get is-active share-data)
      (begin
        ;; Deactivate share
        (map-set file-shares
          { share-id: share-id }
          (merge share-data { is-active: false })
        )
        
        ;; Remove from active shares
        (map-delete active-file-shares { file-id: (get file-id share-data), renter: (get renter share-data) })
        
        ;; Update owner earnings
        (let (
          (current-earnings (default-to { total-earned: u0, active-rentals: u0, total-rentals: u0 }
            (map-get? owner-earnings { owner: (get owner share-data) })))
        )
          (map-set owner-earnings
            { owner: (get owner share-data) }
            (merge current-earnings { active-rentals: (- (get active-rentals current-earnings) u1) })
          )
        )
        (ok true)
      )
      (ok false) ;; Already cleaned up
    )
  )
)

;; Read-only functions
(define-read-only (get-file-share (share-id uint))
  (map-get? file-shares { share-id: share-id })
)

(define-read-only (get-active-share (file-id uint) (renter principal))
  (map-get? active-file-shares { file-id: file-id, renter: renter })
)

(define-read-only (is-share-valid (share-id uint))
  (match (map-get? file-shares { share-id: share-id })
    share-data (let (
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
      (and
        (get is-active share-data)
        (<= current-time (get expiry-time share-data))
        (match (get max-access-count share-data)
          max-count (< (get access-count share-data) max-count)
          true
        )
      )
    )
    false
  )
)

(define-read-only (get-owner-earnings (owner principal))
  (map-get? owner-earnings { owner: owner })
)

(define-read-only (get-file-rental-settings (file-id uint))
  (map-get? file-rental-settings { file-id: file-id })
)

(define-read-only (get-file-usage-stats (file-id uint))
  (map-get? file-usage-stats { file-id: file-id })
)

(define-read-only (calculate-rental-cost (file-id uint) (duration-hours uint))
  (match (map-get? file-rental-settings { file-id: file-id })
    settings (let (
      (base-cost (* duration-hours (get hourly-rate settings)))
      (platform-fee (/ (* base-cost (var-get platform-fee-percentage)) u100))
    )
      (some { total-cost: base-cost, platform-fee: platform-fee, owner-receives: (- base-cost platform-fee) })
    )
    none
  )
)

(define-read-only (get-platform-fee-percentage)
  (var-get platform-fee-percentage)
)

;; Admin function to update platform fee
(define-public (update-platform-fee (new-percentage uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-percentage u20) ERR-INVALID-PRICE) ;; Max 20%
    (var-set platform-fee-percentage new-percentage)
    (ok true)
  )
)
