(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_FILE_NOT_FOUND (err u101))
(define-constant ERR_FILE_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_HASH (err u103))
(define-constant ERR_TRANSFER_FAILED (err u104))
(define-constant ERR_NOT_OWNER (err u105))

(define-data-var next-file-id uint u1)

(define-map files
  { file-id: uint }
  {
    owner: principal,
    file-hash: (buff 32),
    file-name: (string-ascii 256),
    file-size: uint,
    timestamp: uint,
    description: (string-ascii 512),
    is-public: bool
  }
)

(define-map file-hash-to-id
  { file-hash: (buff 32) }
  { file-id: uint }
)

(define-map user-files
  { owner: principal, file-id: uint }
  { exists: bool }
)

(define-map file-access
  { file-id: uint, accessor: principal }
  { granted: bool, granted-at: uint }
)

(define-public (register-file 
  (file-hash (buff 32))
  (file-name (string-ascii 256))
  (file-size uint)
  (description (string-ascii 512))
  (is-public bool))
  (let
    (
      (file-id (var-get next-file-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-none (map-get? file-hash-to-id { file-hash: file-hash })) ERR_FILE_ALREADY_EXISTS)
    (asserts! (> (len file-hash) u0) ERR_INVALID_HASH)
    
    (map-set files
      { file-id: file-id }
      {
        owner: tx-sender,
        file-hash: file-hash,
        file-name: file-name,
        file-size: file-size,
        timestamp: current-time,
        description: description,
        is-public: is-public
      }
    )
    
    (map-set file-hash-to-id
      { file-hash: file-hash }
      { file-id: file-id }
    )
    
    (map-set user-files
      { owner: tx-sender, file-id: file-id }
      { exists: true }
    )
    
    (var-set next-file-id (+ file-id u1))
    (ok file-id)
  )
)

(define-public (transfer-ownership (file-id uint) (new-owner principal))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender (get owner file-data)) ERR_NOT_OWNER)
    
    (map-delete user-files { owner: tx-sender, file-id: file-id })
    
    (map-set user-files
      { owner: new-owner, file-id: file-id }
      { exists: true }
    )
    
    (map-set files
      { file-id: file-id }
      (merge file-data { owner: new-owner })
    )
    
    (ok true)
  )
)

(define-public (grant-access (file-id uint) (accessor principal))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender (get owner file-data)) ERR_NOT_OWNER)
    
    (map-set file-access
      { file-id: file-id, accessor: accessor }
      { granted: true, granted-at: current-time }
    )
    
    (ok true)
  )
)

(define-public (revoke-access (file-id uint) (accessor principal))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner file-data)) ERR_NOT_OWNER)
    
    (map-delete file-access { file-id: file-id, accessor: accessor })
    
    (ok true)
  )
)

(define-public (update-file-visibility (file-id uint) (is-public bool))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner file-data)) ERR_NOT_OWNER)
    
    (map-set files
      { file-id: file-id }
      (merge file-data { is-public: is-public })
    )
    
    (ok true)
  )
)

(define-read-only (get-file-info (file-id uint))
  (map-get? files { file-id: file-id })
)

(define-read-only (get-file-by-hash (file-hash (buff 32)))
  (match (map-get? file-hash-to-id { file-hash: file-hash })
    file-id-data (map-get? files { file-id: (get file-id file-id-data) })
    none
  )
)

(define-read-only (verify-ownership (file-id uint) (claimed-owner principal))
  (match (map-get? files { file-id: file-id })
    file-data (is-eq (get owner file-data) claimed-owner)
    false
  )
)

(define-read-only (verify-file-hash (file-id uint) (claimed-hash (buff 32)))
  (match (map-get? files { file-id: file-id })
    file-data (is-eq (get file-hash file-data) claimed-hash)
    false
  )
)

(define-read-only (has-access (file-id uint) (accessor principal))
  (let
    (
      (file-data (map-get? files { file-id: file-id }))
    )
    (match file-data
      file-info
        (or
          (is-eq accessor (get owner file-info))
          (get is-public file-info)
          (default-to false (get granted (map-get? file-access { file-id: file-id, accessor: accessor })))
        )
      false
    )
  )
)

(define-read-only (get-file-timestamp (file-id uint))
  (match (map-get? files { file-id: file-id })
    file-data (some (get timestamp file-data))
    none
  )
)

(define-read-only (is-file-owner (file-id uint))
  (match (map-get? files { file-id: file-id })
    file-data (is-eq tx-sender (get owner file-data))
    false
  )
)

(define-read-only (get-next-file-id)
  (var-get next-file-id)
)

(define-read-only (file-exists (file-id uint))
  (is-some (map-get? files { file-id: file-id }))
)

(define-read-only (hash-exists (file-hash (buff 32)))
  (is-some (map-get? file-hash-to-id { file-hash: file-hash }))
)

(define-read-only (get-access-info (file-id uint) (accessor principal))
  (map-get? file-access { file-id: file-id, accessor: accessor })
)