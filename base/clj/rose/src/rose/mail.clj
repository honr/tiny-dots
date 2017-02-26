(ns rose.mail
  ;; don't :use this ns.
  (:refer-clojure :exclude [send])
  (:import [javax.mail 
	    Address 
	    Message
	    Message$RecipientType
	    Session] 

	   [javax.mail.internet 
	    MimeMessage
	    MimeBodyPart
	    MimeMultipart
	    InternetAddress]
	   [java.util Date]
	   [com.sun.mail.smtp SMTPTransport]))

(defn auth [k]
  "select a header from user's auth file which is at
      ~/.config/cl/auth.clj for GNU/Linux, and at
      ~/Library/cl/auth.clj for Mac OS X"
  (get (read-string 
	(slurp 
	 (str
	  (System/getProperty "user.dir") "/"
	  (cond (= (System/getProperty "os.name") "Linux") 
		".config"
		(= (System/getProperty "os.name") "Mac OS X")
		"Library")
	  "/cl/auth.clj"))) k))

(defn send [header text & files] 
  "Send an email and attach files if supplied.
   header is a map with the following fields:
    :subject Subject
    :to destination email address
    :cc carbon-copy email address, defaults to nil
    :bcc hidden carbon-copy email address defaults to nil
    :from source email address
    :mailhost mailhost address
    :mailhost-port mailhost port number
    :mailer MTA name string
    :username username to authenticate on the mailhost
    :password password
   text is the body of the email
   if files are supplied, they will be attached to the email."

  (let [props (System/getProperties)
	header (merge 
		{:subject "SUBJECT"
		 :to "TO-FIELD"
		 :cc nil
		 :bcc nil
		 :from "FROM-FIELD" 
		 :mailhost "mailhost"
		 :mailhost-port "587"
		 :mailer "a Midget on a Cloud"
		 :username "USER"
		 :password "PASSWORD"}
		header)]
   (.put props "mail.smtps.host" (:mailhost header))
   (.put props "mail.smtps.port" (:mailhost-port header))
   (.put props "mail.smtps.auth" "true")
   (let [session (Session/getInstance props nil)
	 msg (MimeMessage. session)]
     (.setFrom msg (InternetAddress. (:from header))) ;; (.setFrom msg)
     (.setRecipients msg Message$RecipientType/TO
		     (InternetAddress/parse (:to header) false))
     (when (:cc header) 
       (.setRecipients msg Message$RecipientType/CC
		       (InternetAddress/parse (:cc header) false)))
     (when (:bcc header)
       (.setRecipients msg Message$RecipientType/BCC 
		       (InternetAddress/parse (:bcc header) false)))
     (.setSubject msg (:subject header))
     (if files
       (let [mime-body-part-text (MimeBodyPart.)
	     mime-part   (MimeMultipart.)]
	 (.setText mime-body-part-text text)
	 (.addBodyPart mime-part mime-body-part-text)
	 (doseq [file files]
	   (let [mime-body-part-file (MimeBodyPart.)]
	     (.attachFile mime-body-part-file #^String file)
	     (.addBodyPart mime-part mime-body-part-file)))
	 (.setContent msg mime-part))
       (.setText msg text)) ; if the desired charset is known, you can use 
					; (.setText text charset)
     (.setHeader msg "X-Mailer" (:mailer header))
     (.setSentDate msg (Date.))

     ;; send the message
     (let [transport #^SMTPTransport (.getTransport session "smtps")]
       (try
	
	(do 
	  (.connect transport 
		    (:mailhost header) 
		    (:username header)
		    (:password header))
	    ;; (if (:auth header)
	    ;;   (.connect transport 
	    ;; 		(:mailhost header) (:user header) (:password header))
	    ;;   (.connect transport))
	  (.sendMessage transport msg (.getAllRecipients msg)))
	{:error :SUCCESS :message "sent."}
	(catch Exception e
	  {:error (class e) :message (.getMessage e)}))))))
