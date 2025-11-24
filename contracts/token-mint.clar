;; token-mint.clar
;; Simple fungible token with owner-controlled minting.
;; - First caller should call `initialize` to set the owner/minter (one-time).
;; - After initialization only owner can mint.
;; - Supports transfer and balance query.
;; Note: This is intentionally simple for learning/demo purposes.

(define-constant ERR-NOT-INITIALIZED u100)
(define-constant ERR-ALREADY_INITIALIZED u101)
(define-constant ERR-NOT-OWNER u102)
(define-constant ERR-ZERO_AMOUNT u103)
(define-constant ERR-INSUFFICIENT_BALANCE u104)
(define-constant ERR-OVERFLOW u105)

;; Owner stored as optional principal; initially none.
(define-data-var owner (optional principal) none)

;; Total supply stored as uint
(define-data-var total-supply uint u0)

;; Balances map: key { account: principal } -> value { balance: uint }
(define-map balances
  { account: principal }
  { balance: uint })

;; ---------- Helpers ----------

;; internal: get stored balance for an account (returns uint)
(define-read-only (get-balance (account principal))
  (ok (match (map-get? balances { account: account })
         balance-data (get balance balance-data)
         u0)))

;; internal: set balance for an account


;; internal: safe-add that checks overflow (we test monotonic increase)
(define-private (safe-add (a uint) (b uint))
  (let ((sum (+ a b)))
    (if (>= sum a) ;; if sum >= a then no overflow (Clarity int wrap isn't permitted; this is a sanity check)
        (ok sum)
        (err ERR-OVERFLOW))))

;; ---------- Initialization ----------

;; initialize owner (one-time). First caller (tx-sender) OR a provided principal can become owner.
;; Caller must call this immediately after deploy to set the owner/minter.
(define-public (initialize (owner-principal principal))
  (let ((current-owner (var-get owner)))
    (if (is-some current-owner)
        (err ERR-ALREADY_INITIALIZED)
        (begin
            (asserts! (is-some (some owner-principal)) (err ERR-NOT-INITIALIZED))
            (var-set owner (some owner-principal))
            (ok owner-principal)))))

;; read-only: returns optional owner
(define-read-only (get-owner)
  (ok (var-get owner)))

;; ---------- Minting ----------

;; Mint tokens to recipient. Only owner (set by initialize) can mint.
(define-public (mint (recipient principal) (amount uint))
  (begin
    (asserts! (not (is-eq amount u0)) (err ERR-ZERO_AMOUNT))
    (let ((current-owner (var-get owner)))
      (asserts! (not (is-none current-owner)) (err ERR-NOT-INITIALIZED))
      (let ((owner-principal (unwrap-panic current-owner))
            (checked-recipient recipient))
        (asserts! (and (is-eq tx-sender owner-principal)
                      (is-some (some checked-recipient))) 
                 (err ERR-NOT-OWNER))
        (let ((cur-total (var-get total-supply))
              (cur-balance (default-to u0 
                          (get balance (map-get? balances { account: checked-recipient })))))
          (match (safe-add cur-total amount)
            err-result (err err-result)
            new-total (match (safe-add cur-balance amount)
                      err-result (err err-result)
                      new-balance (begin
                                  (map-set balances 
                                          { account: checked-recipient } 
                                          { balance: new-balance })
                                  (var-set total-supply new-total)
                                  (ok new-total)))))))))

;; ---------- Transfer ----------

;; Transfer `amount` from tx-sender to recipient.
(define-public (transfer (recipient principal) (amount uint))
  (let ((sender tx-sender)
        (checked-recipient recipient))
    (asserts! (and (not (is-eq amount u0))
                   (is-some (some checked-recipient)))
              (err ERR-ZERO_AMOUNT))
    (let ((sender-balance 
           (get balance (default-to {balance: u0} (map-get? balances { account: sender }))))
          (recipient-balance 
           (get balance (default-to {balance: u0} (map-get? balances { account: checked-recipient })))))
      (if (< sender-balance amount)
          (err ERR-INSUFFICIENT_BALANCE)
          (begin
            (map-set balances 
                    { account: sender } 
                    { balance: (- sender-balance amount) })
            (map-set balances 
                    { account: checked-recipient } 
                    { balance: (+ recipient-balance amount) })
            (ok true))))))

;; ---------- Read-only total supply ----------

(define-read-only (get-total-supply)
  (ok (var-get total-supply)))
