(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_FILE_NOT_FOUND (err u101))
(define-constant ERR_FILE_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_HASH (err u103))
(define-constant ERR_TRANSFER_FAILED (err u104))
(define-constant ERR_NOT_OWNER (err u105))
(define-constant ERR_COLLECTION_NOT_FOUND (err u106))
(define-constant ERR_COLLECTION_ALREADY_EXISTS (err u107))
(define-constant ERR_FILE_NOT_IN_COLLECTION (err u108))
(define-constant ERR_COLLECTION_LIMIT_EXCEEDED (err u109))
(define-constant ERR_BATCH_OPERATION_FAILED (err u110))
(define-constant ERR_VERSION_NOT_FOUND (err u111))
(define-constant ERR_VERSION_ALREADY_EXISTS (err u112))
(define-constant ERR_INVALID_VERSION_NUMBER (err u113))
(define-constant ERR_CANNOT_DELETE_CURRENT_VERSION (err u114))
(define-constant ERR_VERSION_LIMIT_EXCEEDED (err u115))

(define-data-var next-file-id uint u1)
(define-data-var next-collection-id uint u1)
(define-data-var next-version-id uint u1)

(define-map files
  { file-id: uint }
  {
    owner: principal,
    file-hash: (buff 32),
    file-name: (string-ascii 256),
    file-size: uint,
    timestamp: uint,
    description: (string-ascii 512),
    is-public: bool,
    current-version: uint,
    version-count: uint
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

(define-map collections
  { collection-id: uint }
  {
    owner: principal,
    name: (string-ascii 128),
    description: (string-ascii 256),
    created-at: uint,
    file-count: uint,
    is-public: bool
  }
)

(define-map collection-files
  { collection-id: uint, file-id: uint }
  { added-at: uint }
)

(define-map user-collections
  { owner: principal, collection-id: uint }
  { exists: bool }
)

(define-map collection-access
  { collection-id: uint, accessor: principal }
  { granted: bool, granted-at: uint }
)

(define-map file-versions
  { file-id: uint, version-number: uint }
  {
    version-id: uint,
    file-hash: (buff 32),
    file-size: uint,
    timestamp: uint,
    version-notes: (string-ascii 512),
    is-current: bool,
    previous-version: (optional uint),
    tags: (list 10 (string-ascii 32))
  }
)

(define-map version-metadata
  { version-id: uint }
  {
    file-id: uint,
    version-number: uint,
    creator: principal,
    created-at: uint,
    change-summary: (string-ascii 256),
    approval-status: (string-ascii 32),
    rollback-count: uint
  }
)

(define-map file-version-history
  { file-id: uint }
  {
    versions: (list 50 uint),
    total-versions: uint,
    last-updated: uint
  }
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
        is-public: is-public,
        current-version: u1,
        version-count: u1
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
    
    (let
      (
        (version-id (var-get next-version-id))
      )
      (map-set file-versions
        { file-id: file-id, version-number: u1 }
        {
          version-id: version-id,
          file-hash: file-hash,
          file-size: file-size,
          timestamp: current-time,
          version-notes: "Initial version",
          is-current: true,
          previous-version: none,
          tags: (list)
        }
      )
      
      (map-set version-metadata
        { version-id: version-id }
        {
          file-id: file-id,
          version-number: u1,
          creator: tx-sender,
          created-at: current-time,
          change-summary: "File created",
          approval-status: "approved",
          rollback-count: u0
        }
      )
      
      (map-set file-version-history
        { file-id: file-id }
        {
          versions: (list version-id),
          total-versions: u1,
          last-updated: current-time
        }
      )
      
      (var-set next-version-id (+ version-id u1))
      (var-set next-file-id (+ file-id u1))
      (ok file-id)
    )
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

(define-public (create-collection
  (name (string-ascii 128))
  (description (string-ascii 256))
  (is-public bool))
  (let
    (
      (collection-id (var-get next-collection-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (map-set collections
      { collection-id: collection-id }
      {
        owner: tx-sender,
        name: name,
        description: description,
        created-at: current-time,
        file-count: u0,
        is-public: is-public
      }
    )
    
    (map-set user-collections
      { owner: tx-sender, collection-id: collection-id }
      { exists: true }
    )
    
    (var-set next-collection-id (+ collection-id u1))
    (ok collection-id)
  )
)

(define-public (add-file-to-collection (collection-id uint) (file-id uint))
  (let
    (
      (collection-data (unwrap! (map-get? collections { collection-id: collection-id }) ERR_COLLECTION_NOT_FOUND))
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender (get owner collection-data)) ERR_NOT_OWNER)
    (asserts! (is-none (map-get? collection-files { collection-id: collection-id, file-id: file-id })) ERR_FILE_ALREADY_EXISTS)
    
    (map-set collection-files
      { collection-id: collection-id, file-id: file-id }
      { added-at: current-time }
    )
    
    (map-set collections
      { collection-id: collection-id }
      (merge collection-data { file-count: (+ (get file-count collection-data) u1) })
    )
    
    (ok true)
  )
)

(define-public (remove-file-from-collection (collection-id uint) (file-id uint))
  (let
    (
      (collection-data (unwrap! (map-get? collections { collection-id: collection-id }) ERR_COLLECTION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner collection-data)) ERR_NOT_OWNER)
    (asserts! (is-some (map-get? collection-files { collection-id: collection-id, file-id: file-id })) ERR_FILE_NOT_IN_COLLECTION)
    
    (map-delete collection-files { collection-id: collection-id, file-id: file-id })
    
    (map-set collections
      { collection-id: collection-id }
      (merge collection-data { file-count: (- (get file-count collection-data) u1) })
    )
    
    (ok true)
  )
)

(define-public (transfer-collection-ownership (collection-id uint) (new-owner principal))
  (let
    (
      (collection-data (unwrap! (map-get? collections { collection-id: collection-id }) ERR_COLLECTION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner collection-data)) ERR_NOT_OWNER)
    
    (map-delete user-collections { owner: tx-sender, collection-id: collection-id })
    
    (map-set user-collections
      { owner: new-owner, collection-id: collection-id }
      { exists: true }
    )
    
    (map-set collections
      { collection-id: collection-id }
      (merge collection-data { owner: new-owner })
    )
    
    (ok true)
  )
)

(define-public (grant-collection-access (collection-id uint) (accessor principal))
  (let
    (
      (collection-data (unwrap! (map-get? collections { collection-id: collection-id }) ERR_COLLECTION_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq tx-sender (get owner collection-data)) ERR_NOT_OWNER)
    
    (map-set collection-access
      { collection-id: collection-id, accessor: accessor }
      { granted: true, granted-at: current-time }
    )
    
    (ok true)
  )
)

(define-public (revoke-collection-access (collection-id uint) (accessor principal))
  (let
    (
      (collection-data (unwrap! (map-get? collections { collection-id: collection-id }) ERR_COLLECTION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner collection-data)) ERR_NOT_OWNER)
    
    (map-delete collection-access { collection-id: collection-id, accessor: accessor })
    
    (ok true)
  )
)

(define-public (batch-grant-file-access (file-ids (list 20 uint)) (accessor principal))
  (let
    (
      (batch-result (map batch-grant-access-helper file-ids))
    )
    (asserts! (is-eq (len (filter is-ok-response batch-result)) (len file-ids)) ERR_BATCH_OPERATION_FAILED)
    (ok (len file-ids))
  )
)

(define-public (batch-revoke-file-access (file-ids (list 20 uint)) (accessor principal))
  (let
    (
      (batch-result (map batch-revoke-access-helper file-ids))
    )
    (asserts! (is-eq (len (filter is-ok-response batch-result)) (len file-ids)) ERR_BATCH_OPERATION_FAILED)
    (ok (len file-ids))
  )
)

(define-public (batch-transfer-files (file-ids (list 20 uint)) (new-owner principal))
  (let
    (
      (batch-result (map batch-transfer-helper file-ids))
    )
    (asserts! (is-eq (len (filter is-ok-response batch-result)) (len file-ids)) ERR_BATCH_OPERATION_FAILED)
    (ok (len file-ids))
  )
)

(define-private (batch-grant-access-helper (file-id uint))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (if (is-eq tx-sender (get owner file-data))
      (begin
        (map-set file-access
          { file-id: file-id, accessor: tx-sender }
          { granted: true, granted-at: current-time }
        )
        (ok true)
      )
      ERR_NOT_OWNER
    )
  )
)

(define-private (batch-revoke-access-helper (file-id uint))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
    )
    (if (is-eq tx-sender (get owner file-data))
      (begin
        (map-delete file-access { file-id: file-id, accessor: tx-sender })
        (ok true)
      )
      ERR_NOT_OWNER
    )
  )
)

(define-private (batch-transfer-helper (file-id uint))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
      (new-owner tx-sender)
    )
    (if (is-eq tx-sender (get owner file-data))
      (begin
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
      ERR_NOT_OWNER
    )
  )
)

(define-private (is-ok-response (response (response bool uint)))
  (is-ok response)
)

(define-read-only (get-collection-info (collection-id uint))
  (map-get? collections { collection-id: collection-id })
)

(define-read-only (is-file-in-collection (collection-id uint) (file-id uint))
  (is-some (map-get? collection-files { collection-id: collection-id, file-id: file-id }))
)

(define-read-only (has-collection-access (collection-id uint) (accessor principal))
  (let
    (
      (collection-data (map-get? collections { collection-id: collection-id }))
    )
    (match collection-data
      collection-info
        (or
          (is-eq accessor (get owner collection-info))
          (get is-public collection-info)
          (default-to false (get granted (map-get? collection-access { collection-id: collection-id, accessor: accessor })))
        )
      false
    )
  )
)

(define-read-only (get-collection-file-count (collection-id uint))
  (match (map-get? collections { collection-id: collection-id })
    collection-data (some (get file-count collection-data))
    none
  )
)

(define-read-only (get-next-collection-id)
  (var-get next-collection-id)
)

(define-read-only (collection-exists (collection-id uint))
  (is-some (map-get? collections { collection-id: collection-id }))
)

(define-read-only (is-collection-owner (collection-id uint))
  (match (map-get? collections { collection-id: collection-id })
    collection-data (is-eq tx-sender (get owner collection-data))
    false
  )
)

(define-read-only (get-collection-access-info (collection-id uint) (accessor principal))
  (map-get? collection-access { collection-id: collection-id, accessor: accessor })
)

(define-read-only (get-file-collection-info (collection-id uint) (file-id uint))
  (map-get? collection-files { collection-id: collection-id, file-id: file-id })
)

(define-public (create-file-version
  (file-id uint)
  (file-hash (buff 32))
  (file-size uint)
  (version-notes (string-ascii 512))
  (change-summary (string-ascii 256))
  (tags (list 10 (string-ascii 32))))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (version-id (var-get next-version-id))
      (new-version-number (+ (get version-count file-data) u1))
      (current-version-number (get current-version file-data))
    )
    (asserts! (is-eq tx-sender (get owner file-data)) ERR_NOT_OWNER)
    (asserts! (<= new-version-number u50) ERR_VERSION_LIMIT_EXCEEDED)
    (asserts! (> (len file-hash) u0) ERR_INVALID_HASH)
    
    (map-set file-versions
      { file-id: file-id, version-number: current-version-number }
      (merge (unwrap-panic (map-get? file-versions { file-id: file-id, version-number: current-version-number })) { is-current: false })
    )
    
    (map-set file-versions
      { file-id: file-id, version-number: new-version-number }
      {
        version-id: version-id,
        file-hash: file-hash,
        file-size: file-size,
        timestamp: current-time,
        version-notes: version-notes,
        is-current: true,
        previous-version: (some current-version-number),
        tags: tags
      }
    )
    
    (map-set version-metadata
      { version-id: version-id }
      {
        file-id: file-id,
        version-number: new-version-number,
        creator: tx-sender,
        created-at: current-time,
        change-summary: change-summary,
        approval-status: "pending",
        rollback-count: u0
      }
    )
    
    (let
      (
        (history-data (default-to { versions: (list), total-versions: u0, last-updated: u0 } (map-get? file-version-history { file-id: file-id })))
        (updated-versions (unwrap-panic (as-max-len? (append (get versions history-data) version-id) u50)))
      )
      (map-set file-version-history
        { file-id: file-id }
        {
          versions: updated-versions,
          total-versions: new-version-number,
          last-updated: current-time
        }
      )
    )
    
    (map-set files
      { file-id: file-id }
      (merge file-data { 
        current-version: new-version-number, 
        version-count: new-version-number,
        file-hash: file-hash,
        file-size: file-size,
        timestamp: current-time
      })
    )
    
    (var-set next-version-id (+ version-id u1))
    (ok version-id)
  )
)

(define-public (rollback-to-version (file-id uint) (target-version-number uint))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
      (target-version (unwrap! (map-get? file-versions { file-id: file-id, version-number: target-version-number }) ERR_VERSION_NOT_FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (current-version-number (get current-version file-data))
    )
    (asserts! (is-eq tx-sender (get owner file-data)) ERR_NOT_OWNER)
    (asserts! (not (is-eq target-version-number current-version-number)) ERR_INVALID_VERSION_NUMBER)
    (asserts! (<= target-version-number (get version-count file-data)) ERR_VERSION_NOT_FOUND)
    
    (map-set file-versions
      { file-id: file-id, version-number: current-version-number }
      (merge (unwrap-panic (map-get? file-versions { file-id: file-id, version-number: current-version-number })) { is-current: false })
    )
    
    (map-set file-versions
      { file-id: file-id, version-number: target-version-number }
      (merge target-version { is-current: true, timestamp: current-time })
    )
    
    (let
      (
        (target-metadata (unwrap-panic (map-get? version-metadata { version-id: (get version-id target-version) })))
      )
      (map-set version-metadata
        { version-id: (get version-id target-version) }
        (merge target-metadata { rollback-count: (+ (get rollback-count target-metadata) u1) })
      )
    )
    
    (map-set files
      { file-id: file-id }
      (merge file-data { 
        current-version: target-version-number,
        file-hash: (get file-hash target-version),
        file-size: (get file-size target-version),
        timestamp: current-time
      })
    )
    
    (ok true)
  )
)

(define-public (delete-version (file-id uint) (version-number uint))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
      (version-data (unwrap! (map-get? file-versions { file-id: file-id, version-number: version-number }) ERR_VERSION_NOT_FOUND))
      (current-version-number (get current-version file-data))
    )
    (asserts! (is-eq tx-sender (get owner file-data)) ERR_NOT_OWNER)
    (asserts! (not (is-eq version-number current-version-number)) ERR_CANNOT_DELETE_CURRENT_VERSION)
    (asserts! (> (get version-count file-data) u1) ERR_INVALID_VERSION_NUMBER)
    
    (map-delete file-versions { file-id: file-id, version-number: version-number })
    (map-delete version-metadata { version-id: (get version-id version-data) })
    
    (ok true)
  )
)

(define-public (approve-version (file-id uint) (version-number uint))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
      (version-data (unwrap! (map-get? file-versions { file-id: file-id, version-number: version-number }) ERR_VERSION_NOT_FOUND))
      (metadata (unwrap! (map-get? version-metadata { version-id: (get version-id version-data) }) ERR_VERSION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner file-data)) ERR_NOT_OWNER)
    
    (map-set version-metadata
      { version-id: (get version-id version-data) }
      (merge metadata { approval-status: "approved" })
    )
    
    (ok true)
  )
)

(define-public (reject-version (file-id uint) (version-number uint))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
      (version-data (unwrap! (map-get? file-versions { file-id: file-id, version-number: version-number }) ERR_VERSION_NOT_FOUND))
      (metadata (unwrap! (map-get? version-metadata { version-id: (get version-id version-data) }) ERR_VERSION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner file-data)) ERR_NOT_OWNER)
    
    (map-set version-metadata
      { version-id: (get version-id version-data) }
      (merge metadata { approval-status: "rejected" })
    )
    
    (ok true)
  )
)

(define-public (update-version-tags (file-id uint) (version-number uint) (new-tags (list 10 (string-ascii 32))))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
      (version-data (unwrap! (map-get? file-versions { file-id: file-id, version-number: version-number }) ERR_VERSION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner file-data)) ERR_NOT_OWNER)
    
    (map-set file-versions
      { file-id: file-id, version-number: version-number }
      (merge version-data { tags: new-tags })
    )
    
    (ok true)
  )
)

(define-read-only (get-file-version (file-id uint) (version-number uint))
  (map-get? file-versions { file-id: file-id, version-number: version-number })
)

(define-read-only (get-version-metadata (version-id uint))
  (map-get? version-metadata { version-id: version-id })
)

(define-read-only (get-file-version-history (file-id uint))
  (map-get? file-version-history { file-id: file-id })
)

(define-read-only (get-current-version (file-id uint))
  (match (map-get? files { file-id: file-id })
    file-data (map-get? file-versions { file-id: file-id, version-number: (get current-version file-data) })
    none
  )
)

(define-read-only (get-version-count (file-id uint))
  (match (map-get? files { file-id: file-id })
    file-data (some (get version-count file-data))
    none
  )
)

(define-read-only (compare-versions (file-id uint) (version1 uint) (version2 uint))
  (let
    (
      (v1-data (map-get? file-versions { file-id: file-id, version-number: version1 }))
      (v2-data (map-get? file-versions { file-id: file-id, version-number: version2 }))
    )
    (match v1-data
      version1-info
        (match v2-data
          version2-info
            (some {
              version1-size: (get file-size version1-info),
              version2-size: (get file-size version2-info),
              size-difference: (if (> (get file-size version1-info) (get file-size version2-info))
                (- (get file-size version1-info) (get file-size version2-info))
                (- (get file-size version2-info) (get file-size version1-info))
              ),
              time-difference: (if (> (get timestamp version1-info) (get timestamp version2-info))
                (- (get timestamp version1-info) (get timestamp version2-info))
                (- (get timestamp version2-info) (get timestamp version1-info))
              ),
              hash-match: (is-eq (get file-hash version1-info) (get file-hash version2-info))
            })
          none
        )
      none
    )
  )
)

(define-read-only (get-versions-by-tag (file-id uint) (tag (string-ascii 32)))
  (let
    (
      (history (map-get? file-version-history { file-id: file-id }))
    )
    (match history
      history-data
        (filter version-has-tag (get versions history-data))
      (list)
    )
  )
)

(define-private (version-has-tag (version-id uint))
  (match (map-get? version-metadata { version-id: version-id })
    metadata
      (let
        (
          (file-id (get file-id metadata))
          (version-number (get version-number metadata))
        )
        (match (map-get? file-versions { file-id: file-id, version-number: version-number })
          version-data
            (> (len (filter is-target-tag (get tags version-data))) u0)
          false
        )
      )
    false
  )
)

(define-private (is-target-tag (tag (string-ascii 32)))
  (is-eq tag tag)
)

(define-read-only (get-next-version-id)
  (var-get next-version-id)
)


