{-# LANGUAGE OverloadedStrings #-}

-- | The privacy policy corpus.
--
-- The section list below is the table of contents, in order.
--
-- Remember the house rule from "Privgen.Document": a clause body must not
-- branch on whether it should appear. Every inclusion decision belongs in the
-- condition. If a clause needs two shapes, write two clauses.
module Privgen.Corpus.Privacy (privacyDocument) where

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Text.Blaze.Html             (Html, toHtml)
import qualified Text.Blaze.Html5            as H

import           Privgen.Catalog
import           Privgen.Condition
import           Privgen.Corpus.Common
import           Privgen.Document
import           Privgen.Env
import           Privgen.Render
import           Privgen.Spec
import           Privgen.Types

privacyDocument :: Document
privacyDocument = Document
  { docKind = PrivacyDoc
  , docSections =
      [ introSection
      , whoWeAreSection
      , whatWeCollectSection
      , legalBasesSection
      , childrenSection
      , recipientsSection
      , advertisingSection
      , transfersSection
      , retentionSection
      , securitySection
      , rightsGdprSection
      , rightsUsSection
      , rightsKvkkSection
      , dataDeletionSection
      , thirdPartyLinksSection
      , changesSection
      , contactSection
      ]
  }

-- ---------------------------------------------------------------------------

inEurope :: Condition
inEurope = AnyOf [InRegion EEA, InRegion UK, InRegion Switzerland]

inUs :: Condition
inUs = AnyOf [InRegion CaliforniaUS, InRegion OtherStateUS]

reachesChildren :: Condition
reachesChildren = AnyOf [ForAudience ChildDirected, ForAudience MixedAudience]

-- | Every catalogued SDK is either a processor or a controller, so the absence
-- of both means the game ships none at all.
hasNoSdks :: Condition
hasNoSdks =
  Not (AnyOf [AnySdkWith IsProcessor, AnySdkWith ActsAsController])

-- | Renders a representative block. The @Nothing@ arm is unreachable for any
-- selected clause, because the conditions that guard these clauses require the
-- representative to be present.
representative :: Text -> Maybe Representative -> Html
representative _     Nothing  = pure ()
representative intro (Just r) = do
  H.p (toHtml intro)
  addressLines ([repName r] <> repAddress r)
  H.p (email_ (repEmail r))

-- ---------------------------------------------------------------------------

introSection :: Section
introSection = section "intro" "About this policy"
  [ clause "privacy.intro" Always (\env -> do
      p_ ("This policy explains what "
          <> specName (envSpec env)
          <> " collects, why, who else receives it, and what you can do about \
             \it. We have tried to write it in plain language rather than the \
             \usual boilerplate.")
      p_ "Where a section applies only to people in a particular place, it \
         \says so in its heading.")
  , disclaimerNote
  , clause "privacy.terms-link" Always (\env -> do
      H.p $ do
        "Words defined in our Terms of Service have the same meaning here. \
        \You can read them at "
        let s = unSlug (specSlug (envSpec env))
        link_ ("/terms/" <> s) ("/terms/" <> s)
        "."
    )
  ]

whoWeAreSection :: Section
whoWeAreSection = section "who-we-are" "Who we are"
  [ companyIdentity
  , clause "privacy.eu-representative"
      (AllOf [InRegion EEA, HasEuRepresentative]) (\env ->
        representative "Our representative in the European Union for data \
                       \protection matters is:"
                       (coEuRep (specCompany (envSpec env)))
      ) `citing` ["GDPR Art. 27"]
  , clause "privacy.uk-representative"
      (AllOf [InRegion UK, HasUkRepresentative]) (\env ->
        representative "Our representative in the United Kingdom is:"
                       (coUkRep (specCompany (envSpec env)))
      ) `citing` ["UK GDPR Art. 27"]
  , clause "privacy.verbis"
      (AllOf [InRegion Turkey, HasVerbisNumber]) (\env ->
        maybe (pure ())
          (\n -> p_ ("We are registered with the Turkish Data Protection \
                     \Authority's controllers' registry (VERBIS) under \
                     \number " <> n <> "."))
          (coVerbis (specCompany (envSpec env)))
      ) `citing` ["KVKK m.16"]
  ]

-- ---------------------------------------------------------------------------

whatWeCollectSection :: Section
whatWeCollectSection = section "what-we-collect" "What we collect and why"
  [ clause "privacy.collection-table" Always (\env -> do
      p_ "Each row below is a category of data, what it is used for, and who \
         \receives it. Anything not listed here, we do not collect."
      defList_ (map row (collectionTable env)))
      `citing` [ "GDPR Art. 13(1)(c)", "GDPR Art. 14(1)(d)"
               , "CCPA \167 1798.100(b)", "KVKK m.10" ]
  , clause "privacy.no-account" (Not (HasFeature AccountsFeature)) (\_ ->
      p_ "The game has no accounts and no sign-in. We never ask for your \
         \name, your email address, or anything else that identifies you \
         \directly.")
  , clause "privacy.account" (HasFeature AccountsFeature) (\_ ->
      p_ "If you create an account, we also hold the details you give us when \
         \you do, and the progress attached to it.")
  , clause "privacy.no-sensitive" Always (\_ ->
      p_ "We do not collect special category or sensitive personal data \8212 \
         \nothing about health, biometrics, race, religion, sexuality, \
         \politics, or union membership. We do not ask for it and the game \
         \has no use for it.")
      `citing` ["GDPR Art. 9", "CPRA \167 1798.121", "KVKK m.6"]
  ]
  where
    row (cat, purposes, receivers) =
      ( toHtml (dataCategoryName cat)
      , do
          toHtml ("Used for " <> joinPhrases (map purposeName purposes) <> ".")
          case receivers of
            [] -> " Not shared with anyone."
            rs -> toHtml (" Received by " <> commaList (map sdkName rs) <> ".")
      )

-- ---------------------------------------------------------------------------

legalBasesSection :: Section
legalBasesSection =
  (section "legal-bases" "Our legal bases (EEA, UK and Switzerland)"
    [ clause "privacy.legal-bases" Always (\env -> do
        p_ "Data protection law in Europe requires us to have a specific \
           \legal basis for each use of your data. Ours are:"
        defList_ (bases env))
        `citing` [ "GDPR Art. 6(1)(a)", "GDPR Art. 6(1)(b)"
                 , "GDPR Art. 6(1)(f)", "GDPR Art. 13(1)(d)" ]
    , clause "privacy.consent-withdrawal" DeclaresAds (\_ ->
        p_ "Where we rely on your consent \8212 which is the case for \
           \personalised advertising \8212 you can withdraw it at any time \
           \in the game's privacy settings, or through your device's \
           \advertising controls. Withdrawing it does not affect anything \
           \done beforehand, and the game keeps working; you will simply see \
           \untargeted ads instead.")
        `citing` ["GDPR Art. 7(3)"]
    ]) { secWhen = inEurope }
  where
    bases env = concat
      [ [ ( "Performance of a contract"
          , "Running the game, saving your progress, and handling your \
            \purchases. Without this we cannot provide the game at all." ) ]
      , [ ( "Legitimate interests"
          , "Understanding how the game is played so we can fix and improve \
            \it, and detecting cheating and fraud. We have weighed this \
            \against your interests and use the least identifying data that \
            \will do the job." ) ]
      , [ ( "Consent"
          , "Personalised advertising and the identifiers that make it \
            \possible. You are asked separately, and you can say no." )
        | declaresAds env ]
      , [ ( "Legal obligation"
          , "Keeping records we are required by law to keep, such as tax \
            \records for purchases." )
        | declaresIap env ]
      ]

-- ---------------------------------------------------------------------------

childrenSection :: Section
childrenSection = section "children" "Children"
  [ clause "privacy.children.general"
      (ForAudience GeneralAudience) (\env ->
        p_ ("The game is not directed to children. We do not knowingly \
            \collect personal information from children under "
            <> tshow (childAge env)
            <> ". If you believe a child has given us their information, \
               \write to us and we will delete it."))
      `citing` ["COPPA 16 CFR 312", "GDPR Art. 8"]
  , clause "privacy.children.mixed" (ForAudience MixedAudience) (\_ ->
      p_ "The game is enjoyed by players of all ages, so we ask for your age \
         \when you first play. We ask in a neutral way that does not \
         \encourage you to give a false answer, and we remember it so you are \
         \not asked twice.")
      `citing` ["COPPA 16 CFR 312.5"]
  , clause "privacy.children.directed" (ForAudience ChildDirected) (\_ ->
      p_ "The game is directed to children, and we treat every player as a \
         \child accordingly.")
      `citing` ["COPPA 16 CFR 312.2"]
  , clause "privacy.children.no-behavioural" reachesChildren (\_ ->
      p_ "For players we know or treat as children, advertising is \
         \contextual only. We do not use their data to build a profile, we \
         \do not allow behavioural or personalised advertising, and we \
         \configure every advertising service we use to treat the traffic as \
         \child-directed.")
      `citing` ["COPPA 16 CFR 312.2", "COPPA 16 CFR 312.4"]
  , clause "privacy.children.consent" (ForAudience ChildDirected) (\env -> do
      p_ "We collect from children only what the game needs in order to run. \
         \Before we would disclose a child's personal information to anyone \
         \outside our own operation, we ask a parent for consent separately \
         \from any other permission \8212 agreeing to one is not agreeing to \
         \the other."
      H.p $ do
        "A parent or guardian can ask to see what we hold about their child, \
        \have it deleted, or tell us to stop collecting anything further, by \
        \writing to "
        email_ (coPrivacyEmail (specCompany (envSpec env)))
        ". We will verify that the request comes from the child's parent \
        \before acting on it."
    ) `citing` [ "COPPA 16 CFR 312.5(a)(2)", "COPPA 16 CFR 312.6" ]
  , clause "privacy.children.retention" reachesChildren (\env ->
      p_ ("We keep a child's personal information only for as long as it is \
          \needed for the purpose it was collected for, and then delete it. \
          \Analytics data is kept for "
          <> retAnalytics (specRetention (envSpec env))
          <> ". We do not keep children's data indefinitely."))
      `citing` ["COPPA 16 CFR 312.10"]
  , clause "privacy.children.eu" (AllOf [inEurope, Not (ForAudience GeneralAudience)]) (\_ ->
      p_ "In the EEA and the UK the age at which a young person can consent \
         \on their own behalf is higher than in the United States \8212 \
         \between 13 and 16 depending on the country. Where consent is \
         \needed below that age, we ask a parent.")
      `citing` ["GDPR Art. 8(1)"]
  ]
  where
    childAge env = maybe (13 :: Int) id (specAgeGate (envSpec env))

-- ---------------------------------------------------------------------------

recipientsSection :: Section
recipientsSection = section "recipients" "Who else receives your data"
  [ clause "privacy.recipients.none" hasNoSdks (\_ ->
      p_ "Nobody. The game does not send your data to any third party.")
  , clause "privacy.recipients.processors-only"
      (AllOf [AnySdkWith IsProcessor, Not (AnySdkWith ActsAsController)]) (\_ ->
        p_ "Every service we use acts only on our instructions. None of them \
           \may use your data for their own purposes.")
  , clause "privacy.processors" (AnySdkWith IsProcessor) (\env -> do
      p_ "These services process data on our behalf and under our \
         \instructions. They may not use it for anything else:"
      defList_ (map vendorRow (processors env)))
      `citing` ["GDPR Art. 28"]
  , clause "privacy.controllers" (AnySdkWith ActsAsController) (\env -> do
      p_ "These companies decide for themselves how they use the data they \
         \receive, so they are responsible for it in their own right. We \
         \cannot answer for them, and you will need to take any request about \
         \their processing to them directly:"
      defList_ (map vendorRow (controllers env)))
      `citing` ["GDPR Art. 13(1)(e)", "GDPR Art. 26"]
  ]

vendorRow :: SdkEntry -> (Html, Html)
vendorRow e =
  ( toHtml (sdkName e)
  , do
      toHtml (sdkVendor e <> " \8212 ")
      link_ (sdkPrivacyUrl e) "privacy policy"
      case sdkContact e of
        Nothing -> pure ()
        Just c  -> H.br >> email_ c
  )

-- ---------------------------------------------------------------------------

advertisingSection :: Section
advertisingSection = section "advertising" "Advertising"
  [ clause "privacy.ads.how" DeclaresAds (\_ ->
      p_ "The game is free, and advertising is how it pays for itself. \
         \Advertising services use an identifier from your device to decide \
         \which ads to show and to count whether you saw them.")
  , clause "privacy.ads.rewarded" (Monetizes RewardedAds) (\_ ->
      p_ "Some ads are optional: you can choose to watch one in exchange for \
         \something in the game. Watching is always your choice, and \
         \declining costs you nothing you had already earned.")
  , clause "privacy.ads.ios" (AllOf [DeclaresAds, OnPlatform IOS]) (\_ ->
      p_ "On iOS, personalised advertising depends on the App Tracking \
         \Transparency prompt. If you decline, we do not access your device's \
         \advertising identifier and the ads you see are untargeted. You can \
         \change your mind in Settings \8594 Privacy & Security \8594 Tracking.")
  , clause "privacy.ads.android" (AllOf [DeclaresAds, OnPlatform Android]) (\_ ->
      p_ "On Android you can reset or delete your advertising ID at any time \
         \in Settings \8594 Privacy \8594 Ads. Deleting it stops personalised \
         \advertising across every app on the device.")
  , clause "privacy.ads.eu-consent" (AllOf [DeclaresAds, inEurope]) (\_ ->
      p_ "In Europe we ask for your consent before any personalised \
         \advertising, through a consent prompt the first time you play. You \
         \can change your answer at any time in the game's privacy settings.")
      `citing` ["ePrivacy Directive Art. 5(3)", "GDPR Art. 6(1)(a)"]
  ]

-- ---------------------------------------------------------------------------

transfersSection :: Section
transfersSection =
  (section "international-transfers" "Sending data abroad"
    [ clause "privacy.transfers" Always (\env -> do
        p_ "Some of the services we use are based outside the country you \
           \are in, so your data may be processed elsewhere. Where that \
           \happens, we rely on the following safeguards:"
        ul_ [ toHtml (transferBasisName b) | b <- transferBases env ]
        H.p $ do
          "You can ask us for a copy of the relevant safeguard by writing to "
          email_ (coPrivacyEmail (specCompany (envSpec env)))
          ".")
        `citing` [ "GDPR Art. 13(1)(f)", "GDPR Art. 46", "KVKK m.9" ]
    , clause "privacy.transfers.kvkk"
        (AllOf [InRegion Turkey, AnySdkWith TransfersInternationally]) (\_ ->
        p_ "For transfers out of T\252rkiye we rely on the mechanisms in \
           \Article 9 of the KVKK as amended in 2024: an adequacy decision \
           \where one exists, and otherwise a standard contract notified to \
           \the Personal Data Protection Authority within five business days \
           \of signature.")
        `citing` ["KVKK m.9", "Regulation of 10 July 2024"]
    ])
    { secWhen = AnySdkWith TransfersInternationally }

-- ---------------------------------------------------------------------------

retentionSection :: Section
retentionSection = section "retention" "How long we keep things"
  [ clause "privacy.retention" Always (\env ->
      let r = specRetention (envSpec env)
      in do
        p_ "We keep data only as long as it is useful for the purpose it was \
           \collected for, and then delete or anonymise it."
        defList_ $
          [ ("Gameplay and usage events", toHtml (retAnalytics r))
          , ("Crash reports and diagnostics", toHtml (retCrash r))
          , ("Support correspondence", toHtml (retSupport r))
          ] <> [ ("Account data", toHtml a) | Just a <- [retAccount r] ])
      `citing` ["GDPR Art. 13(2)(a)", "COPPA 16 CFR 312.10"]
  ]

securitySection :: Section
securitySection = section "security" "Keeping it safe"
  [ clause "privacy.security" Always (\_ ->
      p_ "Data in transit is encrypted. Access to anything we store is \
         \limited to the people who need it to do their job. No system is \
         \perfectly secure, and we will not pretend otherwise \8212 but we \
         \do not collect what we do not need, which is the most effective \
         \protection available.")
      `citing` ["GDPR Art. 32", "KVKK m.12"]
  ]

-- ---------------------------------------------------------------------------

rightsGdprSection :: Section
rightsGdprSection =
  (section "rights-gdpr" "Your rights (EEA, UK and Switzerland)"
    [ clause "privacy.rights.gdpr" Always (\_ -> do
        p_ "You have the right to:"
        ul_ (map toHtml gdprRights)
        p_ "We answer within one month. Exercising any of these is free, and \
           \we will not treat you differently for it.")
        `citing` [ "GDPR Arts. 15-22", "GDPR Art. 12(3)" ]
    , clause "privacy.rights.gdpr.complain" Always (\_ ->
        p_ "If you think we have got something wrong, please tell us first \
           \\8212 but you are entitled to complain to your national data \
           \protection authority whether or not you come to us, and nothing \
           \here is meant to discourage that.")
        `citing` ["GDPR Art. 77"]
    , clause "privacy.rights.gdpr.automated" Always (\_ ->
        p_ "We do not make decisions about you by automated means that have \
           \a legal or similarly significant effect.")
        `citing` ["GDPR Art. 22"]
    ]) { secWhen = inEurope }
  where
    gdprRights :: [Text]
    gdprRights =
      [ "ask what we hold about you and get a copy of it"
      , "have anything inaccurate corrected"
      , "have your data deleted"
      , "ask us to restrict how we use it while a dispute is resolved"
      , "receive what you gave us in a portable form, or have it sent \
        \onward to someone else"
      , "object to processing we base on legitimate interests"
      , "withdraw any consent you have given, at any time"
      ]

rightsUsSection :: Section
rightsUsSection =
  (section "rights-us" "Your rights (United States)"
    [ clause "privacy.rights.us" Always (\_ -> do
        p_ "Depending on where you live in the United States, you may have \
           \the right to know what we collect, to get a copy of it, to have \
           \it corrected or deleted, and to opt out of its sale or sharing. \
           \We extend all of these to every US player regardless of state, \
           \because maintaining fifty versions of the truth helps nobody."
        p_ "We will not deny you service, charge you a different price, or \
           \give you a worse experience for exercising any of them.")
        `citing` [ "CCPA \167 1798.100", "CCPA \167 1798.105"
                 , "CCPA \167 1798.106", "CCPA \167 1798.125" ]
    , clause "privacy.rights.us.sale-share"
        (AnySdkWith SellsOrShares) (\env -> do
          p_ "Some advertising services receive identifiers from the game in \
             \a way that counts as a \8220sale\8221 or a \8220share\8221 \
             \under California law, even though no money changes hands for \
             \your data. Those services are:"
          defList_ (map optOutRow (saleOrShareSdks env))
          p_ "You can turn this off in the game's privacy settings under \
             \\8220Do Not Sell or Share My Personal Information\8221, which \
             \applies immediately and needs no account.")
        `citing` [ "CCPA \167 1798.120", "CPRA \167 1798.121" ]
    , clause "privacy.rights.us.gpc" (AnySdkWith SellsOrShares) (\_ ->
        p_ "We honour the Global Privacy Control and other recognised \
           \opt-out preference signals. If your browser or device sends one, \
           \we treat it as an opt-out without your having to ask again.")
        `citing` ["CCPA Regulations \167 7025"]
    , clause "privacy.rights.us.minors"
        (AllOf [AnySdkWith SellsOrShares, reachesChildren]) (\_ ->
        p_ "We do not sell or share the personal information of players we \
           \know to be under 16.")
        `citing` ["CCPA \167 1798.120(c)"]
    , clause "privacy.rights.us.appeal" Always (\env -> do
        H.p $ do
          "If we refuse a request, we will tell you why, and you can appeal \
          \by replying to our decision or writing to "
          email_ (coPrivacyEmail (specCompany (envSpec env)))
          " with \8220appeal\8221 in the subject line. We will respond \
          \within 45 days."
        p_ "You can also authorise someone else to make a request for you; \
           \we will need to verify both their authority and your identity \
           \before we act.")
        `citing` [ "Va. Code \167 59.1-577(C)", "Colo. Rev. Stat. \167 6-1-1306(3)" ]
    ]) { secWhen = inUs }

optOutRow :: SdkEntry -> (Html, Html)
optOutRow e =
  ( toHtml (sdkName e)
  , case sdkOptOutUrl e of
      Just u  -> link_ u "opt out directly"
      Nothing -> link_ (sdkPrivacyUrl e) "privacy policy"
  )

rightsKvkkSection :: Section
rightsKvkkSection =
  (section "rights-kvkk" "Your rights (T\252rkiye)"
    [ clause "privacy.rights.kvkk" Always (\env -> do
        p_ "Under Article 11 of the KVKK you may ask us whether we process \
           \your personal data, what we do with it, who we transfer it to at \
           \home and abroad, and to have it corrected, deleted or destroyed. \
           \You may also object to a result produced solely by automated \
           \analysis, and claim compensation for damage caused by unlawful \
           \processing."
        H.p $ do
          "Applications must be made in writing or by another method the \
          \Authority permits, and we respond within 30 days at the latest. \
          \Write to "
          email_ (coPrivacyEmail (specCompany (envSpec env)))
          ". If you are not satisfied with our answer, you may complain to \
          \the Personal Data Protection Board."
        p_ "This section is provided in English for convenience. A Turkish \
           \aydinlatma metni is available on request and governs in case of \
           \any difference.")
        `citing` [ "KVKK m.11", "KVKK m.13", "KVKK m.14" ]
    ]) { secWhen = InRegion Turkey }

-- ---------------------------------------------------------------------------

-- | The @DataDeletion@ anchor is deep-linked by Google Play's data-deletion
-- requirement. It is pinned by name here, the section's first clause is
-- unconditional so the section can never be suppressed, and a property test
-- asserts the anchor survives for every possible spec.
dataDeletionSection :: Section
dataDeletionSection =
  (section "data-deletion" "Deleting your data"
    [ clause "privacy.deletion.request" Always (\env -> do
        H.p $ do
          "To have the data we hold about you deleted, write to "
          email_ (coPrivacyEmail (specCompany (envSpec env)))
          " from the device or account you play on, or use the deletion \
          \option in the game's settings. You do not need to give a reason."
        p_ "We action deletion requests within 30 days and confirm when it \
           \is done. Some records we are legally required to keep \8212 \
           \purchase records for tax purposes, for example \8212 survive \
           \deletion; we will tell you if any apply to you.")
    , clause "privacy.deletion.uninstall" Always (\_ ->
        p_ "Deleting the game from your device removes everything stored on \
           \the device itself, but does not by itself reach data already \
           \sent to us or to the services listed above. Use the request \
           \route for that.")
    , clause "privacy.deletion.partners"
        (AnySdkWith ActsAsController) (\_ ->
        p_ "The companies listed above that act on their own behalf hold \
           \their own copies, which we cannot delete for you. Each entry \
           \above links to that company's policy and, where they publish \
           \one, a contact address for exactly this kind of request.")
    ]) `anchored` "DataDeletion"

thirdPartyLinksSection :: Section
thirdPartyLinksSection = section "third-party-links" "Links out"
  [ clause "privacy.links" Always (\_ ->
      p_ "The game and its ads sometimes link elsewhere. Once you follow a \
         \link you are on someone else's site, under someone else's policy, \
         \and we have no control over what they do.")
  ]

changesSection :: Section
changesSection = section "changes" "Changes to this policy" [changesClause]

contactSection :: Section
contactSection = section "contact" "Contact us" [contactClause]

-- ---------------------------------------------------------------------------
-- Small text helpers
-- ---------------------------------------------------------------------------

tshow :: Int -> Text
tshow = T.pack . show

commaList :: [Text] -> Text
commaList = joinWith ", " " and "

-- | "a, b and c", falling back sensibly for zero and one item.
joinPhrases :: [Text] -> Text
joinPhrases [] = "the operation of the game"
joinPhrases xs = joinWith ", " " and " xs

joinWith :: Text -> Text -> [Text] -> Text
joinWith sep final xs = case reverse xs of
  []       -> ""
  [x]      -> x
  (x : ys) -> T.intercalate sep (reverse ys) <> final <> x
