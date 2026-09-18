{-# LANGUAGE OverloadedStrings #-}

-- | The terms of service corpus.
--
-- Same machinery as the privacy corpus; only the section list differs. The
-- same house rule applies: inclusion decisions live in conditions, never in
-- clause bodies.
module Privgen.Corpus.Terms (termsDocument) where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Text.Blaze.Html5      as H

import           Privgen.Condition
import           Privgen.Corpus.Common
import           Privgen.Document
import           Privgen.Env
import           Privgen.Render
import           Privgen.Spec
import           Privgen.Types

termsDocument :: Document
termsDocument = Document
  { docKind = TermsDoc
  , docSections =
      [ acceptanceSection
      , eligibilitySection
      , licenceSection
      , accountsSection
      , virtualItemsSection
      , purchasesSection
      , advertisingSection
      , conductSection
      , ugcSection
      , platformSection
      , ipSection
      , availabilitySection
      , disclaimersSection
      , liabilitySection
      , terminationSection
      , governingLawSection
      , changesSection
      , contactSection
      ]
  }

consumerRegions :: Condition
consumerRegions =
  AnyOf [InRegion EEA, InRegion UK, InRegion Switzerland, InRegion Turkey]

-- ---------------------------------------------------------------------------

acceptanceSection :: Section
acceptanceSection = section "acceptance" "Agreeing to these terms"
  [ clause "terms.acceptance" Always (\env -> do
      p_ ("These terms are the agreement between you and us about your use \
          \of " <> specName (envSpec env) <> ". Installing or playing the \
          \game means you accept them.")
      p_ "If you do not accept them, do not play \8212 and if you have \
         \already installed the game, you can simply delete it.")
  , companyIdentity
  , disclaimerNote
  ]

eligibilitySection :: Section
eligibilitySection = section "eligibility" "Who can play"
  [ clause "terms.eligibility" Always (\env ->
      p_ ("You need to be at least "
          <> tshow (tsMinAge (specTerms (envSpec env)))
          <> " to agree to these terms. If you are younger than the age of \
             \majority where you live, a parent or guardian has to agree to \
             \them for you, and by playing you are telling us that has \
             \happened."))
  , clause "terms.eligibility.parents"
      (AnyOf [ForAudience ChildDirected, ForAudience MixedAudience]) (\_ ->
        p_ "Parents and guardians: you are responsible for supervising your \
           \child's use of the game, including any purchases made on your \
           \device. Both app stores offer controls that require approval \
           \before a purchase goes through, and we would encourage you to \
           \use them.")
  ]

licenceSection :: Section
licenceSection = section "licence" "Your licence to play"
  [ clause "terms.licence" Always (\_ ->
      p_ "We give you a personal, non-exclusive, non-transferable, revocable \
         \licence to install and play the game on devices you own or \
         \control, for your own entertainment. We keep ownership of the game \
         \itself; you are not buying it, you are being allowed to use it.")
  , clause "terms.licence.restrictions" Always (\_ -> do
      p_ "There are a few things you may not do:"
      ul_ (map toHtmlText
        [ "sell, rent, sublicense or commercially exploit the game"
        , "reverse engineer, decompile or disassemble it, except where the \
          \law expressly gives you that right despite this clause"
        , "modify it, or use cheats, bots, automation, memory editors or \
          \modified clients"
        , "exploit a bug for advantage instead of reporting it"
        , "interfere with the servers or other players' enjoyment of the game"
        ]))
  ]

accountsSection :: Section
accountsSection =
  (section "accounts" "Accounts and progress"
    [ clause "terms.accounts" (HasFeature AccountsFeature) (\_ ->
        p_ "You are responsible for keeping your account secure and for what \
           \happens under it. Tell us promptly if you think someone else has \
           \access.")
    , clause "terms.accounts.cloud-save"
        (HasFeature CloudSaveFeature) (\_ ->
        p_ "Progress saved to the cloud is a convenience, not a guarantee. \
           \Keep in mind that a device transfer, a reinstall, or a long \
           \period away can lose progress, and we cannot always recover it.")
    ]) { secWhen = AnyOf [HasFeature AccountsFeature, HasFeature CloudSaveFeature] }

-- ---------------------------------------------------------------------------

virtualItemsSection :: Section
virtualItemsSection =
  (section "virtual-items" "Coins, gems and other in-game items"
    [ clause "terms.virtual.licence" Always (\_ ->
        p_ "In-game currency and items are a licence to use a feature of the \
           \game. You do not own them, they have no value outside the game, \
           \and they cannot be exchanged for money or transferred to anyone \
           \else.")
    , clause "terms.virtual.no-cash-value" Always (\_ ->
        p_ "We may change the price of items, the rate at which they are \
           \earned, or how they work, as part of balancing the game. Where a \
           \change would take away something you have already paid for, we \
           \will offer a fair equivalent.")
    , clause "terms.virtual.on-termination" Always (\_ ->
        p_ "Unused items and currency disappear when your licence ends \8212 \
           \whether because you stop playing, because we close your access \
           \for breaking these terms, or because the game shuts down. This \
           \does not affect any refund you are legally entitled to.")
    , clause "terms.virtual.shutdown" Always (\_ ->
        p_ "If we decide to retire the game, we will say so in the game and \
           \stop selling items at that point, giving you a reasonable period \
           \to use what you have already bought.")
    ]) { secWhen = HasVirtualItems }

purchasesSection :: Section
purchasesSection =
  (section "purchases" "Purchases and refunds"
    [ clause "terms.purchases.store" Always (\env -> do
        p_ "Purchases in the game are handled by the app store you \
           \downloaded it from, not by us. They take the payment, they issue \
           \the receipt, and their terms govern the transaction."
        ul_ [ toHtmlText ("Bought through " <> platformName p <> ".")
            | p <- specPlatforms (envSpec env) ])
    , clause "terms.purchases.withdrawal" consumerRegions (\_ -> do
        p_ "If you are a consumer in the EEA, the UK, Switzerland or \
           \T\252rkiye, you normally have 14 days to withdraw from a \
           \purchase of digital content without giving a reason."
        p_ "Because in-game items are delivered immediately, you are asked \
           \to agree at the point of purchase that delivery starts straight \
           \away, and you acknowledge that doing so ends the withdrawal \
           \right once the item has been delivered. Until you receive the \
           \item, the right still stands.")
        `citing` [ "Directive 2011/83/EU Art. 16(m)"
                 , "UK Consumer Contracts Regulations 2013 reg. 37" ]
    , clause "terms.purchases.faulty" consumerRegions (\_ ->
        p_ "Separately from any withdrawal right, if something you bought is \
           \faulty or not as described, you are entitled to a remedy under \
           \consumer law. Nothing in these terms takes that away.")
    , clause "terms.purchases.contact" Always (\env -> do
        H.p $ do
          "For a refund, contact the app store first \8212 they hold the \
          \payment. If they send you back to us, or something has gone wrong \
          \on our side, write to "
          email_ (coSupportEmail (specCompany (envSpec env)))
          " and we will help."
        )
    ]) { secWhen = DeclaresIap }

advertisingSection :: Section
advertisingSection =
  (section "advertising" "Advertising in the game"
    [ clause "terms.ads" Always (\env -> do
        p_ "The game is free to play and shows advertising to pay for \
           \itself. We do not control what each ad says, and showing an ad \
           \is not a recommendation of the product in it."
        H.p $ do
          "What advertising services collect, and how to limit it, is \
          \described in our "
          let s = unSlug (specSlug (envSpec env))
          link_ ("/privacy/" <> s) "Privacy Policy"
          ".")
    , clause "terms.ads.rewarded" (Monetizes RewardedAds) (\_ ->
        p_ "Some ads are optional and give you something in the game in \
           \return for watching. If an ad fails to load or does not finish \
           \through no fault of your own, the reward should still arrive; \
           \tell us if it does not.")
    ]) { secWhen = DeclaresAds }

conductSection :: Section
conductSection = section "conduct" "How to behave"
  [ clause "terms.conduct" Always (\_ ->
      p_ "Play fairly. Do not cheat, do not harass other players, do not try \
         \to break the game or the services behind it, and do not use the \
         \game for anything unlawful.")
  ]

ugcSection :: Section
ugcSection =
  (section "ugc" "Things you submit"
    [ clause "terms.ugc" Always (\_ ->
        p_ "You keep ownership of anything you submit \8212 a nickname, a \
           \message, a custom creation. You give us permission to host and \
           \display it inside the game, which is what makes the feature \
           \work, and nothing more.")
    , clause "terms.ugc.rules" Always (\_ ->
        p_ "Do not submit anything unlawful, abusive, hateful, sexual, or \
           \that impersonates someone else or infringes their rights. We can \
           \remove anything that breaks this rule and restrict the feature \
           \for anyone who keeps doing it.")
    , clause "terms.ugc.chat" (HasFeature InGameChat) (\_ ->
        p_ "Chat is moderated, but not every message is read before it is \
           \sent. If someone is behaving badly, report them in the game.")
    ]) { secWhen = AnyOf [ HasFeature UserGeneratedContent
                         , HasFeature InGameChat ] }

-- ---------------------------------------------------------------------------

-- | Apple requires a custom EULA to carry certain minimum terms, including
-- that Apple is a third-party beneficiary entitled to enforce them.
platformSection :: Section
platformSection = section "platform-terms" "App store terms"
  [ clause "terms.platform.apple" (OnPlatform IOS) (\_ -> do
      p_ "If you downloaded the game from the Apple App Store, the following \
         \also applies. This agreement is between you and us, not Apple, and \
         \we alone are responsible for the game and its content."
      ul_ (map toHtmlText
        [ "Apple has no obligation to provide any support or maintenance for \
          \the game."
        , "If the game fails to conform to any warranty, you may tell Apple \
          \and Apple will refund the purchase price; beyond that, Apple has \
          \no warranty obligation whatsoever."
        , "Apple is not responsible for addressing any claim by you or a \
          \third party relating to the game, including product liability, \
          \any failure to conform to a legal requirement, and claims under \
          \consumer protection or privacy law."
        , "If a third party claims the game infringes their intellectual \
          \property, we, not Apple, are responsible for handling it."
        , "You confirm you are not located in a country subject to a US \
          \Government embargo, and are not on any US Government list of \
          \prohibited or restricted parties."
        , "Apple and its subsidiaries are third-party beneficiaries of these \
          \terms and have the right to enforce them against you."
        ]))
  , clause "terms.platform.google" (OnPlatform Android) (\_ ->
      p_ "If you downloaded the game from Google Play, the Google Play Terms \
         \of Service also apply to that download and to any purchase you \
         \make through it.")
  ]

ipSection :: Section
ipSection = section "ip" "Who owns what"
  [ clause "terms.ip" Always (\env ->
      p_ ("The game \8212 its code, art, sound, characters and name \8212 \
          \belongs to " <> coLegalName (specCompany (envSpec env))
          <> " or to those who licensed it to us, and is protected by \
             \copyright and trade mark law. Nothing in these terms transfers \
             \any of it to you."))
  ]

availabilitySection :: Section
availabilitySection = section "availability" "Availability and changes"
  [ clause "terms.availability" Always (\_ ->
      p_ "We try to keep the game available and working, but we cannot \
         \promise it will always be up or free of faults. We may update it, \
         \change features, or stop offering it altogether.")
  , clause "terms.availability.sunset" Always (\_ ->
      p_ "If we retire the game for good, we will give notice in the game \
         \beforehand where we reasonably can, so you are not taken by \
         \surprise.")
  ]

-- ---------------------------------------------------------------------------

disclaimersSection :: Section
disclaimersSection = section "disclaimers" "What we do and do not promise"
  [ clause "terms.disclaimer" Always (\_ ->
      p_ "The game is provided as it is. Beyond what we promise here and \
         \what the law requires of us, we do not give warranties about it \
         \\8212 in particular we do not promise that it will be \
         \uninterrupted, error-free, or suited to a purpose you have in mind \
         \for it.")
  , clause "terms.disclaimer.consumer" consumerRegions (\_ ->
      p_ "If you are a consumer, you have rights under the law of your own \
         \country that cannot be signed away. Nothing in this section \
         \affects them, and where anything here conflicts with them, those \
         \rights win.")
  ]

liabilitySection :: Section
liabilitySection = section "liability" "Limits on our liability"
  [ clause "terms.liability.carveout" Always (\_ ->
      p_ "Nothing in these terms excludes or limits our liability for death \
         \or personal injury caused by our negligence, for fraud, or for \
         \anything else that cannot lawfully be excluded.")
  , clause "terms.liability.cap" Always (\_ ->
      p_ "Subject to that, our total liability to you arising out of the \
         \game is limited to the greater of the amount you paid us in the \
         \twelve months before the claim arose, or fifty euro. We are not \
         \liable for lost profits, lost data, or losses that were not \
         \reasonably foreseeable when you started playing.")
  , clause "terms.liability.consumer" consumerRegions (\_ ->
      p_ "If you are a consumer, this limit applies only so far as your \
         \national law allows. Your statutory remedies are unaffected.")
  ]

terminationSection :: Section
terminationSection = section "termination" "Ending this agreement"
  [ clause "terms.termination.you" Always (\_ ->
      p_ "You can end this agreement at any time by deleting the game.")
  , clause "terms.termination.us" Always (\_ ->
      p_ "We can suspend or end your access if you break these terms \8212 \
         \for cheating or abuse, for instance. Where it is proportionate to \
         \do so we will warn you first, and we will tell you why unless the \
         \law stops us. If you think we have got it wrong, write to us and a \
         \person will look at it again.")
  ]

governingLawSection :: Section
governingLawSection = section "governing-law" "Law and disputes"
  [ clause "terms.law" Always (\env ->
      let t = specTerms (envSpec env)
      in p_ ("These terms are governed by the law of " <> tsGoverningLaw t
             <> ", and the courts of " <> tsVenue t
             <> " have jurisdiction over any dispute."))
  , clause "terms.law.consumer" consumerRegions (\_ ->
      p_ "If you are a consumer, that choice of law does not deprive you of \
         \the protection of the mandatory rules of the country you live in, \
         \and you can bring proceedings in your own local courts.")
  , clause "terms.law.turkey" (InRegion Turkey) (\_ ->
      p_ "Consumers in T\252rkiye may also apply to the consumer arbitration \
         \committee (t\252ketici hakem heyeti) or the consumer court for the \
         \place where they live.")
  , clause "terms.law.eu-odr" (InRegion EEA) (\_ ->
      p_ "We are not obliged to use an alternative dispute resolution body, \
         \and we do not currently do so \8212 but you may always complain to \
         \a consumer protection authority in your own country.")
  , clause "terms.law.arbitration" ArbitrationElected (\_ -> do
      p_ "If you live in the United States, you and we agree that disputes \
         \will be resolved by binding individual arbitration rather than in \
         \court, and that neither of us will bring a class action."
      p_ "You can opt out of this within 30 days of first accepting these \
         \terms by writing to us and saying so; opting out costs you \
         \nothing and does not affect anything else here. Small claims court \
         \remains available to either of us.")
  , clause "terms.law.informal" Always (\env -> do
      H.p $ do
        "Before anything formal, please write to "
        email_ (coSupportEmail (specCompany (envSpec env)))
        ". Most things are resolved that way.")
  ]

changesSection :: Section
changesSection = section "changes" "Changes to these terms"
  [ changesClause
  , clause "terms.changes.reject" Always (\_ ->
      p_ "If you do not accept a change, your remedy is to stop playing and \
         \delete the game. Continuing to play after a change takes effect \
         \means you accept it.")
  ]

contactSection :: Section
contactSection = section "contact" "Contact us" [contactClause]

-- ---------------------------------------------------------------------------

tshow :: Int -> Text
tshow = T.pack . show

toHtmlText :: Text -> H.Html
toHtmlText = H.toHtml
