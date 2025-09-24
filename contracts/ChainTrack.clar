;; ChainTrack - Supply Chain Verification System
;; Version: 1.0.0
;; Track product authenticity from manufacturer to consumer with quality validation

(define-map products uint {
  manufacturer: principal,
  product-name: (string-utf8 64),
  batch-details: (string-utf8 256),
  production-date: uint,
  facility-location: (string-utf8 64),
  quality-approved: bool
})

(define-map supplier-inventory principal (list 100 uint))
(define-map quality-inspectors principal bool)
(define-data-var product-id-counter uint u0)

;; Error codes
(define-constant err-not-manufacturer (err u500))
(define-constant err-not-inspector (err u501))
(define-constant err-product-not-found (err u502))
(define-constant err-access-denied (err u503))
(define-constant err-inventory-limit-exceeded (err u504))
(define-constant err-invalid-inspector-address (err u505))
(define-constant err-invalid-product-name (err u506))
(define-constant err-invalid-batch-info (err u507))
(define-constant err-invalid-production-date (err u508))
(define-constant err-invalid-facility-name (err u509))
(define-constant err-invalid-product-id (err u510))

;; System administrator for quality control
(define-constant system-admin tx-sender)

;; Register quality inspector
(define-public (register-quality-inspector (inspector principal))
  (begin
    ;; Check if sender is system administrator
    (asserts! (is-eq tx-sender system-admin) err-access-denied)
    
    ;; Validate inspector principal
    (asserts! (not (is-eq inspector 'SP000000000000000000002Q6VF78)) err-invalid-inspector-address)
    
    ;; Add inspector to registry
    (ok (map-set quality-inspectors inspector true))
  ))

;; Register product in supply chain
(define-public (register-supply-product
  (product-name (string-utf8 64))
  (batch-details (string-utf8 256))
  (production-date uint)
  (facility-location (string-utf8 64)))
  (let
    ((product-id (var-get product-id-counter))
     (manufacturer tx-sender)
     (current-inventory (default-to (list) (map-get? supplier-inventory manufacturer))))
    
    ;; Validate inputs
    (asserts! (> (len product-name) u0) err-invalid-product-name)
    (asserts! (> (len batch-details) u0) err-invalid-batch-info)
    (asserts! (> production-date u0) err-invalid-production-date)
    (asserts! (> (len facility-location) u0) err-invalid-facility-name)
    
    ;; Check inventory limit
    (asserts! (< (len current-inventory) u100) err-inventory-limit-exceeded)
    
    ;; Store product information
    (map-set products product-id {
      manufacturer: manufacturer,
      product-name: product-name,
      batch-details: batch-details,
      production-date: production-date,
      facility-location: facility-location,
      quality-approved: false
    })
    
    ;; Update supplier inventory
    (let
      ((updated-inventory (unwrap-panic (as-max-len? (concat (list product-id) current-inventory) u100))))
      (map-set supplier-inventory manufacturer updated-inventory)
    )
    
    ;; Increment product ID counter
    (var-set product-id-counter (+ product-id u1))
    
    (ok product-id)))

;; Approve product quality
(define-public (approve-product-quality (product-id uint))
  (begin
    ;; Validate product ID
    (asserts! (< product-id (var-get product-id-counter)) err-invalid-product-id)
    
    (let
      ((product (unwrap! (map-get? products product-id) err-product-not-found)))
      
      ;; Check if sender is quality inspector
      (asserts! (default-to false (map-get? quality-inspectors tx-sender)) err-not-inspector)
      
      ;; Update product quality approval status
      (ok (map-set products product-id (merge product {quality-approved: true})))
    )
  ))

;; Get product details
(define-read-only (get-product (product-id uint))
  (map-get? products product-id))

;; Get supplier inventory
(define-read-only (get-supplier-inventory (supplier principal))
  (default-to (list) (map-get? supplier-inventory supplier)))

;; Check quality inspector status
(define-read-only (is-quality-inspector (address principal))
  (default-to false (map-get? quality-inspectors address)))

;; Get total products
(define-read-only (get-total-products)
  (var-get product-id-counter))

;; Get system stats
(define-read-only (get-system-stats)
  {
    admin: system-admin,
    total-products: (var-get product-id-counter)
  })