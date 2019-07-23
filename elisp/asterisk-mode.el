;; asterisk mode
;; notes
;; quick hack of syntax highlighting for asterisk dialplan el file originally posted by Michael Taht
;; this is a crude enhancement but better than pages of text all the same colour ... appeal to experts in emacs/lisp/regex syntax to please improve
(setq max-specpdl-size 6000)
(define-generic-mode 'asterisk-mode
  ;; comments
  '(";")
  ;; keywords
  ;; these are all case sensitive ... asterisk isn't would be nice to fix
  '("AbsoluteTimeout" "AddQueueMember" "ADSIProg" "Agent" "AgentCallBackLogin" "AgentLogin"
    "AgentMonitorOutgoing" "AGI" "Answer" "AppendCDRUserField" "Authenticate"
    "Background" "Busy" "CallingPres" "ChangeMonitor" "ChanIsAvail"
    "Congestion" "Curl" "Cut" "DateTime" "DBdel" "DBdeltree"
    "DBget" "DBput" "DeadAGI" "Dial" "DigitTimeout"
    "Directory" "DISA" "EAGI" "Echo" "EnumLookup"
    "Eval" "Festival" "Flash" "GetCPEID" "Goto"
    "GotoIf" "GotoIfTime" "Gosub" "Gosubif"
    "Hangup" "HasNewVoicemail" "ICES"
    "LookupBlacklist" "LookupCIDName" "Macro" "MailboxExists" "MeetMe"
    "MeetMeCount" "Milliwatt" "Monitor" "MP3Player" "MusicOnHold"
    "NBScat" "NoCDR" "NoOp" "ParkAndAnnounce" "ParkedCall"
    "Playback" "Playtones" "Prefix" "PrivacyManager" "Queue"
    "Random" "Read" "Record" "RemoveQueueMember" "ResetCDR" "Return"
    "ResponseTimeout" "Ringing" "SayDigits" "SayNumber" "SayUnixTime"
    "SendDTMF" "SendImage" "SendURL" "SetAccount" "SetCallerID"
    "SetCDRUserField" "SetCIDName" "SetCIDNum" "SetGlobalVar" "SetLanguage"
    "SetMusicOnHold" "Set" "SetVar" "SIPDtmfMode" "SoftHangup" "StopMonitor"
    "StopPlaytones" "StripLSD" "StripMSD" "SubString" "Suffix" "Swift"
    "System" "Transfer" "VoiceMail" "Voicemail" "VoiceMail2"
    "VoiceMailMain" "VoicemailMain"
    "VoiceMailMain2" "Wait" "WaitExten" "WaitForRing" "WaitMusicOnHold" "Zapateller"
    "ZapBarge" "ZapRAS" "ZapScan"
    )
  ;; colours for the important bits ... need to find more faces and have
  ;; highlights within highlights ... ie ${} within $[ ]
  '(
    ("^\[[a-zA-Z0-9-_]+\]" . 'font-lock-function-name-face)
    ("^exten=>[a-zA-Z0-9_\.#]*,[0-9n\+]*," . 'font-lock-constant-face )
    ("^exten => [a-zA-Z0-9_\.#]*,[0-9n\+]*," . 'font-lock-constant-face )
    ("^include=>[a-zA-Z0-9_\.#]*" . 'font-lock-constant-face )
    ("[A-Z]+/[A-Z]+/\${[a-zA-Z0-9:\(\)@,_\${}-]+}" . 'font-lock-variable-name-face)
    ("[A-Z]+/\${[a-zA-Z0-9:\(\)@,_\${}-]+}" . 'font-lock-variable-name-face)
    ("[A-Z]+/[0-9]+" . 'font-lock-variable-name-face)
    ("__\${[a-zA-Z0-9:\(\)@,_\${}-]+}" . 'font-lock-variable-name-face)
    ("\${[a-zA-Z0-9:\(\)@,_\${}-]+}" . 'font-lock-variable-name-face)
    )
  ;; files
  ;; left ... was only interested in improving readability of dialplans
  ;; god I hate writing dialplans ... but things are way better since gosubs appeared
  '("adsi.conf$" "cdr_pgsql.conf$" "logger.conf$" "musiconhold.conf$" "rpt.conf$"
    "zapata.conf$" "adtranvofr.conf$" "enum.conf$" "manager.conf$" "oss.conf$"
    "rtp.conf$" "agents.conf$" "extensions.conf$" "extconfig.conf$"
    "meetme.conf$" "parking.conf$"
    "sip.conf$" "alsa.conf$" "festival.conf$"   "mgcp.conf$"   "phone.conf$"
    "skinny.conf$" "asterisk.conf$" "iax.conf$" "modem.conf$" "privacy.conf$"
    "voicemail.conf$" "cdr_odbc.conf$" "indications.conf$" "modules.conf$"
   "queues.conf$" "vpb.conf$")
  nil
  "Mode for editing asterisk config files")
