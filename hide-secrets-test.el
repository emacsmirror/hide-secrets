(require 'ert)
(require 'hide-secrets)  ;; Assuming your code is in hide-secrets.el

;; Test cases
(defconst MAC-test-cases
  '("d8:bb:c1:8b:b9:d1\n"
    "d8-bb-c1-8b-b9-d1\n"
    "d8.bb.c1.8b.b9.d1\n"
    "d8bb.c18b.b9d1\n"))

(defconst IPv4-test-cases
  '("192.168.1.43\n"
    "255.255.255.0\n"
    "8.8.8.8\n"
    "235.243.134.167\n"))

(defconst IPv6-test-cases
  '("fd78:7ca5:32f6:20:3442:c65b:1658:a79e\n"
    "fe80::dabb:c1ff:fe8b:b9d1%eth0/64"
    "::1\n"
    "2a00:1450:4005:802::200e\n"))

(defconst email-test-cases
  '("franz.muster@example.com\n"
    "franz.muster+Y5t/34@example.com\n"
    "café.drinker@example.com\n"))

(defconst name-test-cases
  '("name" "имя" "नाम" "nombre" "nom" "nome" "নাম" "naam" "नाव" "പേര്" "பெயர்" "పేరు" "ਨਾਮ" "नाम" "নাম" "jina" "nom" "නම" "نام" "اسم" "שם" "navn" "nom" "nombre" "nafn" "نام" "nimi" "nome" "imya" "பெயர்" "الاسم" "ім'я" "اسم" "名"))

(defconst password-test-cases
  '("password: u#@o9^t8S*xF6ddWuho9\n"
    "Passwort: clumsily-twirl-regroup\n"
    ))

(defconst hash-sum-test-case
  '("MD5(stdin)= 3255e5acced6e40fc7c73ac6eaa34cdc\n"
    "SHA1(stdin)= 202fae858b370a942ac1eeedb74c3ec1e86ae1ca\n"
    "SHA2-224(stdin)= aa0e1c7a9fdc99b1668a45765924797a72bcaf86362245472876a39a\n"
    "SHA2-256(stdin)= c4e783994a45363f16d9ffcd3876287dcf1571c8e394eaa087d851f4fc12c412\n"
    "SHA3-224(stdin)= d87d91aeb6f478c74b473ed958dba332b26a0d45d8721e38f0261c7d\n"
    "SHA3-256(stdin)= cb64b3f9d9004ac79db659c9d1303285edf701212c49015da2ea4bf99f602874\n"
    "SHA2-512/224(stdin)= c412606e86ecdbd108ba81f5153aa6db2af4c57e31f83139eb59e7bb\n"
    "SHAKE-128(stdin)= 5cb11329cfb0e74156271aa2d64d0318\n"
    "SM3(stdin)= a983a01474b92528107343810c9710d3e06cf5ffbd8d419347a496ba051fddab\n"
    "BLAKE2S-256(stdin)= a7c8a84e067b5aa83c6dc3332d5863729e46e6caf8d9b0469a7c6adeaf9a6e98\n"
    "RIPEMD-160(stdin)= 316c9c03b9f98d3dce3236ff175fca11cae12338\n"))

(defconst private-key-test-case
  '("-----BEGIN OPENSSH PRIVATE KEY-----
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
-----END OPENSSH PRIVATE KEY-----
")
  "Example RSA private key.")

(defconst all-test-cases
  (append MAC-test-cases
          IPv4-test-cases
	  IPv6-test-cases
	  email-test-cases
	  password-test-cases
	  private-key-test-case))

(defun test-overlay-properties (prop value)
  "Check if any overlay in the current buffer has a property PROP with VALUE."
  (let ((overlays (overlays-in (point-min) (point-max)))
        found)
    (dolist (overlay overlays found)
      (when (equal (overlay-get overlay prop) value)
        (setq found t)))))

(ert-deftest test-hide-mac-addresses ()
	     "Test if mac-addresses are hidden."
	     (with-temp-buffer 
	       (erase-buffer)
	       (mapcar 'insert MAC-test-cases)
	       (hide-mac-addresses)
	       (should (test-overlay-properties 'hidden-text t)) 
	       (should (test-overlay-properties 'display "**:**:**:**:**:**"))))


(ert-deftest test-hide-ip-addresses ()
	     "Test if IP addresses are hidden."
	     (with-temp-buffer
	       (erase-buffer)
	       (mapcar 'insert IPv4-test-cases)
	       (mapcar 'insert IPv6-test-cases)
	       (hide-ip-addresses)
	       (should (test-overlay-properties 'hidden-text t)) 
	       (should (test-overlay-properties 'display "***.***.***.***"))
	       (should (test-overlay-properties 'display "****:****:****::****"))))

(ert-deftest test-hide-passwords ()
	     "Test if passwords are hidden."
	     (with-temp-buffer
	       (mapcar 'insert password-test-cases)
	       (hide-passwords)
	       (should (test-overlay-properties 'hidden-text t)) 
	       (should (test-overlay-properties 'display "******"))))

(ert-deftest test-hide-email-addresses ()
	     "Test if email addresses are hidden."
	     (with-temp-buffer
	       (mapcar 'insert email-test-cases)
	       (hide-email-addresses)
	       (should (test-overlay-properties 'hidden-text t)) 
	       (should (test-overlay-properties 'display "******@******"))))


(ert-deftest test-hide-names ()
	     "Test if names are hidden."
	     (with-temp-buffer
	       (mapcar 'insert name-test-cases)
	       (hide-names)
	       (should (test-overlay-properties 'hidden-text t)) 
	       (should (test-overlay-properties 'display "******"))))

(ert-deftest test-hide-private-keys ()
	     "Test if private keys are hidden."
	     (with-temp-buffer
	       (mapcar 'insert private-key-test-case)
	       (hide-private-keys)
	       (mark-whole-buffer)
	       (should (test-overlay-properties 'hidden-text t))
	       (should (test-overlay-properties 'display "******"))
	       ))

(ert-deftest test-hide-hash-sums ()
	     "Test if hash sums are hidden."
	     (with-temp-buffer
	       (mapcar 'insert hash-sum-test-case)
	       (hide-hash-sums)
	       (mark-whole-buffer)
	       (should (test-overlay-properties 'hidden-text t))
	       (should (test-overlay-properties 'display "******"))
	       ))


(ert-deftest test-hide-secrets ()
	     "Test if all secrets are hidden."
	     (with-temp-buffer
	       (mapcar 'insert all-test-cases)
	       (hide-secrets)
	       (should (test-overlay-properties 'hidden-text t)) 
	       (should (test-overlay-properties 'display "***.***.***.***"))
	       (should (test-overlay-properties 'display "******"))
	       (should (test-overlay-properties 'display "******@******"))))
