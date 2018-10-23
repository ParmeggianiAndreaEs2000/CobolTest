       identification division.
       program-id.    cogq54.
       Author.        Andrea Parmeggiani - Eurosystem2000.

      *-----------------------------------------------------------------
      * VIsualizzazione stato controlli
      *-----------------------------------------------------------------
      * Programma clonato da SKVIS1
      *-----------------------------------------------------------------
      *
       environment division.
       configuration section.
       source-computer. pc-sperry.
       object-computer. pc-sperry.
       special-names.
           decimal-point is comma.
      *
       input-output section.
       file-control.
      *
       copy "feurtab.fd".
       copy "cogq54.select".
      *copy "cogq54.select".
      *
       data division.
       file section.
      *
       fd  feurtab
           label record standard.
           copy "cogfiles.cpy".
       copy "cogq54.fd".
      *
       working-storage section.
       78 k-max-ele-tab               value 900.
       copy "cogq54.wrk".
       copy "cogq54.imppredf-wrk".

       78 k-ctrl-salva                  value 1.
       78 k-ctrl-find                   value 2.
       78 k-sf6-elabora                 value 3.

       77 k                             pic 9(04).
       77 i1                            pic 9(04).
       77 j1                            pic 9(04).
       77 si                            pic 9(04).
       77 sj                            pic 9(04).
       01 zeta6                         pic zzzzzz.
        


       77 b-k                           pic 9(04).
       77 b-r                           pic 9(04).
       77 b-b                           pic 9(04).
       77 b-random                      pic 9(06).
       77 g-r-numero                    pic 9(18).

       77 fl-fine-merci                 pic x(01).
      *
       01 tab-maschera.
      * Questa copy contiene i dati modificabili definiti in screens,
      * che quindi non devono essere definiti direttamente in
      * tab-maschera
       copy "cogq54.tm".
         02 tm-dati.
      * Definizione altri campi presenti in maschera ma non gestiti
      * direttamente (es. descrizioni read-only)
           03 tm-stato-controllo        pic x(02).
           03 filler                    pic x(01).
      *
       01 datasis                       pic 9(08).
       01 datasis-r redefines datasis.
         02 sis-aa                      pic 9(04).
         02 sis-mm                      pic 9(02).
         02 sis-gg                      pic 9(02).

      *-------------------------------------------------------------**
      *Dati per la gestione pop-up menu in griglia
      *-------------------------------------------------------------**
      * Primo valore di exception gestito
       78  k-pmg-primo                value 151.
      * Ultimo valore di exception gestito
       78  k-pmg-ultimo               value 199.
      * Elenco valori funzioni
      * Puo' essere integrato con altri t.f. gia' utilizzati nella
      * tool-bar
      *
       77 myResult                      pic s9(09).

       01 w-mdc-merce              pic x(09).
       01 w-mdc-lotto              pic x(30).
       01 w-unpack-doc-key         pic x(29).
       01 w-unpack-e-momag redefines w-unpack-doc-key.
           04 wud-e-riga.
             05 wud-e-testa.
               06 wud-e-mag        pic x(01).
               06 wud-e-ann        pic 9(04).
               06 wud-e-tip        pic x(01).
               06 wud-e-num        pic 9(06).
             05 wud-e-rig          pic 9(04).
           04 wud-e-prog           pic 9(06).
           04 filler               pic x(07).
       01 w-unpack-l-lavor redefines w-unpack-doc-key.
           04 wud-l-lav.
              05 wud-l-ann         pic 9(04).
              05 wud-l-num         pic 9(08).
           04 wud-l-fas            pic 9(04).
           04 wud-l-barra          pic x(01).
           04 wud-l-prog           pic 9(04).
      *
       copy "wgrave.cpy".
       copy "wnscr.cpy".
       copy "wopenf.cpy".
       copy "wstato.cpy".
       copy "cogazien.cpy".
       copy "wgrid.cpy".
       copy "utilgdad.cpy".
       copy "utils52.cpy".
       copy "utils65.cpy".
       copy "utils68.cpy".
       copy "utils93.cpy".
       copy "utilu10.cpy".
       copy "utilu20.cpy".
       copy "utilimppredf.cpy".
       copy "utilgesdate.cpy".
      *
       linkage section.
       copy "wcont.cpy".
       copy "utilcogqs6.cpy".
       copy "wcont1.cpy".

       screen section.
       copy "cogq54-01.scr".
      *
       procedure division using stringhe.
       copy "cogq54.decla".
            .
       io-error-rout. exit.
       end declaratives.
      *
       main section.
       apri.
           move k-program-id             to prog-err w-nome-hlp
           perform z-99-init-program
           perform ctr-abil

           perform rd-azi
           perform open-files
      *******************************************************************
      * Tasti funzione previsti nel programma                           *
      *******************************************************************
           move 1                        to w-presenza-f3-conferma
           move 1                        to w-presenza-f3-aggiorna
           move 1                        to w-presenza-f8-calendario
           move 1                        to w-presenza-sf4-griglia
           move 1                        to w-presenza-sf5-esporta-excel
           move 1                        to w-presenza-cta-aggiungi

      *
           accept datasis              from century-date

           perform x-carica-lingua
      *
      **-----------------------------------------------------**
      ** Creo il menu contestuale "griglia"
      **-----------------------------------------------------**
           perform build-main-popup-grid-det.

      **---------------------------------------------------------------**
      * Caricamento bitmap custom: per  la routine load-bitmap-custom   *
      * devo indicare i nomi dei file delle bitmap e quante bitmap      *
      * custom carico (in w-pbc-idx-handle)                             *
      **---------------------------------------------------------------**
           initialize w-push-button-custom
           move 2                        to w-pbc-idx-handle
           move "ricerca"                to w-pbc-bitmap-file (1)
           move "magazzino"              to w-pbc-bitmap-file (2)
           perform load-bitmap-custom
      **---------------------------------------------------------------**
      * Creazione pulsanti custom: w-pbc-idx-handle mi dice da quale    *
      * file prendere il pulsante; w-pbc-bitmap-number mi dice quale    *
      * bitmap prendere all'interno del file.                           *
      **---------------------------------------------------------------**
           move 1                        to w-pbc-idx-handle
           move 5                        to w-pbc-bitmap-number
           move "Salva impostazione (CTRL-S)"   to w-pbc-title
           move k-fun-ctrl-s             to w-pbc-exception
           perform x-display-push-button-custom
           move 1                        to w-pbc-idx-handle
           move 6                        to w-pbc-bitmap-number
           move "Cerca impostazione (CTRL-F)"   to w-pbc-title
           move k-fun-ctrl-f             to w-pbc-exception
           perform x-display-push-button-custom
           move 2                            to w-pbc-idx-handle
           move 4                            to w-pbc-bitmap-number
           move "Forza elaborazione (S-F6)"  to w-pbc-title
           move k-sf6                        to w-pbc-exception
           perform x-display-push-button-custom
      *
      *
           move "Visualizzazione Stato Controlli" to titolo-maschera
           move k-m-v-size               to m-v-size
           move k-m-h-size               to m-h-size
           perform rd-msk-pop

           display s-mm-1
           perform z-costruisci-controlli
           perform z-prepara-tm-grid

      * Caricamento id colonne griglia
           perform x-trova-col-num-grid-det
           .
       a4.
           initialize tab-maschera
           move "E"                     to tm-tipo
           move "4"                     to tm-stato
           move 20170101                to tm-da-data
           move 20991231                to tm-a-data

      * Questo andrebbe messo in una perform preparatoria
           move "S"                     to w-grid-det-ord-reset

      *-----------------------------------------------------------
      * Imposto l'ordinamento predefinito (in questo caso e' fisso,
      * potrebbe dipendere da una configurazione)
      *-----------------------------------------------------------
           move 2                       to gor-r-pnt
           move gcn-r-data-cns          to gor-r-col (1)
           move k-grid-ord-asc          to gor-r-ord-tipo (1)
           move gcn-r-data              to gor-r-col (2)
           move k-grid-ord-asc          to gor-r-ord-tipo (2)
           perform z-grid-det-componi-ord
           perform z-grid-det-mostra-ord

      *-----------------------------------------------------------
      * All'inizio del programma carico l'eventuale filtro di default
      *-----------------------------------------------------------
           perform z-imppredf-carica-default

           perform x-imposta-conferma
           display s-mm-1
           perform z-carica-controlli
           perform x-attiva-f3-conferma
           if w-pbc-visible (k-ctrl-find) = 0
              move k-ctrl-find          to w-pbc-idx
              perform x-attiva-pbc
           end-if
      *
           .
      *----------------------------------------------------------------*
      * Accept di: Magazzino
      *----------------------------------------------------------------*
       a-tm-tipo.
 
           perform x-aiuto-tm-tipo
 
           initialize util-s60
           perform z-s60-tm-tipo
           modify e-tm-tipo, color = ext-color-controls
 
           perform z-99-exception-form
           evaluate z-exception-prosegui
            when k-exc-esci
              go to fine
            when k-exc-rimani
              go to a-tm-tipo
           end-evaluate
 
           if f-event
              go to x-test-mouse
           end-if
           if f2
              go to a-tm-tipo
           end-if
           if f3
              go to a-richiesta-conferma
           end-if
 
           perform x-controlla-tm-tipo
           if x-f3-ok-parziale = "N"
              move x-f3-msg to wb-msg
              perform vbx-msg-error
              go to a-tm-tipo
           end-if
           .
      *----------------------------------------------------------------*
      * Accept di: Data Partenza
      *----------------------------------------------------------------*
       a-tm-da-data.
 
           perform x-aiuto-tm-da-data
 
           perform x-attiva-f8-calendario
           initialize util-s52
           perform z-s52-tm-da-data
           modify e-tm-da-data, color = ext-color-controls
           perform x-disattiva-f8-calendario
 
           perform z-99-exception-form
           evaluate z-exception-prosegui
            when k-exc-esci
              go to a4
            when k-exc-rimani
              go to a-tm-da-data
           end-evaluate
 
           if f-event
              go to x-test-mouse
           end-if
           if f2
              go to a-tm-tipo
           end-if
           if f3
              go to a-richiesta-conferma
           end-if
 
           perform x-controlla-tm-da-data
           if x-f3-ok-parziale = "N"
              move x-f3-msg to wb-msg
              perform vbx-msg-error
              go to a-tm-da-data
           end-if
           .
      *----------------------------------------------------------------*
      * Accept di: Data Arrivo
      *----------------------------------------------------------------*
       a-tm-a-data.
 
           perform x-aiuto-tm-a-data
 
           perform x-attiva-f8-calendario
           initialize util-s52
           perform z-s52-tm-a-data
           modify e-tm-a-data, color = ext-color-controls
           perform x-disattiva-f8-calendario
 
           perform z-99-exception-form
           evaluate z-exception-prosegui
            when k-exc-esci
              go to a4
            when k-exc-rimani
              go to a-tm-a-data
           end-evaluate
 
           if f-event
              go to x-test-mouse
           end-if
           if f2
              go to a-tm-da-data
           end-if
           if f3
              go to a-richiesta-conferma
           end-if
 
           perform x-controlla-tm-a-data
           if x-f3-ok-parziale = "N"
              move x-f3-msg to wb-msg
              perform vbx-msg-error
              go to a-tm-a-data
           end-if
           .
      *----------------------------------------------------------------*
      * Accept di: Stato Controllo
      *----------------------------------------------------------------*
       a-tm-stato.
 
           perform x-aiuto-tm-stato
 
           initialize util-s60
           perform z-s60-tm-stato
           modify e-tm-stato, color = ext-color-controls
 
           perform z-99-exception-form
           evaluate z-exception-prosegui
            when k-exc-esci
              go to a4
            when k-exc-rimani
              go to a-tm-stato
           end-evaluate
 
           if f-event
              go to x-test-mouse
           end-if
           if f2
              go to a-tm-a-data
           end-if
           if f3
              go to a-richiesta-conferma
           end-if
 
           perform x-controlla-tm-stato
           if x-f3-ok-parziale = "N"
              move x-f3-msg to wb-msg
              perform vbx-msg-error
              go to a-tm-stato
           end-if
           .

      *
       a-richiesta-conferma.
           perform x-imposta-conferma
           if x-f3-ok = "N"
              move x-f3-msg              to wb-msg
              perform vbx-msg
              go to a-tm-stato
           end-if
           .

      *
      *-------------------------------------------------------------------
      *-------------------------------------------------------------------
      *-------------------------------------------------------------------
      * Anche questa parte potrebbe essere costruita da screens
      * introducendo eventualmente un entry-point?
      *-------------------------------------------------------------------
      *-------------------------------------------------------------------
      *-------------------------------------------------------------------

      *-------------------------------------------------------------------
      * Inizializzazione file di transito e caricamento 
      *-------------------------------------------------------------------
       a-leggi-dati.
           perform z-grid-det-apri-transito

           perform z-grid-det-crea-transito
           perform z-grid-det-riordina-transito

      **-----------------------------------------------------------------**
      **-----------------------------------------------------------------**
      ** Inizio gestione griglia
      **-----------------------------------------------------------------**
      **-----------------------------------------------------------------**
           perform x-disattiva-f3-conferma
           move spaces                   to gor-r-fl-read
           initialize t-gor-r-rec
           .
       a-grid-ini.
           perform z-grid-det-carica-tab
           if gor-r-n-m = 0
              move spaces                to gor-r-fl-read
              initialize t-gor-r-rec
              perform z-grid-det-carica-tab
           end-if
           .
       a-grid.
           perform x-attiva-tf-grid
           modify e-grid-det, enabled = 1
           perform z-99-accept-grid-det
           perform z-99-exception-form
           evaluate z-exception-prosegui
            when k-exc-esci
              move 0                    to i
              perform x-disattiva-tf-grid
              perform x-gestione-tf-grid-var
              go to a4
            when k-exc-rimani
              go to a-grid
            when k-exc-ricarica
              go to a-leggi-dati
           end-evaluate
           if f-event
              go to x-test-mouse
           end-if
           if wesc
              go to a-grid-fine
           end-if

           if f3
              perform b-aggiorna
              move "Aggiornamento note e data consegna eseguito." 
                                        to wb-msg
              perform vbx-msg-info
              go to a-leggi-dati
           end-if
      *
           if s-f4
              initialize util-gdad
              move prog-err                to gdad-prg
              move titolo-maschera         to gdad-prg-des
              move 1                       to gdad-frm
              move k-id-grid-det           to gdad-ctrl-id
              call "GRIDADMN" using stringhe util-gdad
              go to a-grid
           end-if

           if s-f6 and
              w-pbc-visible (k-sf6-elabora) = 1
              perform a1-elabora
              go to a-grid-ini
           end-if

           if s-f5
              perform z-esporta-grid-det
              go to a-grid
           end-if

      *---------------------------------------------------------------**
      * Aggiungo una riga: solo se non lo ho appena fatto
      * Inserisco un record vuoto sul file di transito; lo visualizzo e
      * mi ci posiziono
      *---------------------------------------------------------------**
           if ctrl-a 
              perform z-grid-det-nuova-riga
           end-if

      *---------------------------------------------------------------**
      * Intercetto le richieste provenienti dal menu pop-up
      *---------------------------------------------------------------**
           evaluate funzio
            when k-pmg-primo thru k-pmg-ultimo
              perform x-attiva-popup-grid-det
              perform x-gestione-tf-grid-var
              evaluate z-exception-prosegui
               when k-exc-rimani
                 go to a-grid
               when k-exc-ricarica
                 go to a-grid-ini
               when k-exc-rileggi
                 go to a-leggi-dati
              end-evaluate
           end-evaluate

           perform z-usa-tf-grid-det
           go to a-grid
           .
       a-grid-fine.
           perform x-disattiva-tf-grid
           go to a4
           .
      *
       fine.
       z-chiudi.
           perform z-close-files
           close t-grid-det
           delete t-grid-det
           perform x-destroy-menu-grid-det
           close window w-sv-cur-prg
           perform z-99-exit-program
           exit program
           .

      *-----------------------------------------------------------------
      * Elaborazione: puo' avvenire anche se il movimento non risulta
      * ancora controllato (il controllo non e' stato ancora effettuato,
      * o non e' stato ancora trasferito da Q3)
      * In questo caso prima di elaborare forzo i dati come se il
      * controllo fosse gia' avvenuto e la quantita' fosse tutta ok
      * Se invece viene impostata gia' una quantita' ok, tengo buona
      * questa
      *-----------------------------------------------------------------
       a1-elabora.
           if tm-r-stato-cnt (i) = "IC"
              move "Il movimento non risulta ancora controllato; prosegu
      -          "o?"                   to wb-msg
              perform acc-conf-custom
              if not f3
                 exit paragraph
              end-if
           end-if
        
      * Se l'utente modifica la quantita' vale questa
           initialize mdc-rec
           move tm-r-doc-tip (i)  to mdc-doc-tip
           move tm-r-doc-key (i)  to mdc-doc-key
           perform rd-clcmovdc
           if w-verbo-ok
              move "CC"              to mdc-stato-controllo
              if tm-r-qta-ok (i) > 0
                 compute tm-r-qta-ko (i) 
                    = tm-r-qta-dec (i) - tm-r-qta-ok (i)
               else
                 move tm-r-qta-dec (i) to tm-r-qta-ok (i)
                 move 0                to tm-r-qta-ko (i)
              end-if
              move tm-r-qta-ok (i)   to mdc-qta-ok
              move tm-r-qta-ko (i)   to mdc-qta-ko
              perform rwr-clcmovdc
              unlock clcmovdc
           end-if

           initialize util-cogqs6
           move "crea-mov-da-mdc"    to cogqs6-ope
           move tm-r-doc-tip (i)     to cogqs6-i-doc-tip
           move tm-r-doc-key (i)     to cogqs6-i-doc-key
           evaluate tm-r-stato-cnt (i)
              when "NC"  move "CN"       to cogqs6-i-new-stato
              when "NT"  move "CN"       to cogqs6-i-new-stato
              when other move "CC"       to cogqs6-i-new-stato
           end-evaluate
           move k-program-id         to cogqs6-caller
           move "COGQS6"             to cogqs6-called
           move k-no                 to cogqs6-i-fl-clock
           call "COGQS6" using stringhe
           if (cogqs6-o-esito = k-no)
              string
                "Fallita chiamata a cogqs6 x chiave:"
                k-newline  delimited size
                 tm-r-doc-tip (i) " " delimited size
                 tm-r-doc-key (i) delimited "   "
                k-newline  delimited size
                "coggqs6 msg:" delimited size
                cogqs6-o-msg delimited "   "
                  into wb-msg
              perform vbx-msg
           end-if

      *-----------------------------------------------------------------
      * Dopo l'elaborazione ricarico il record di transito e la riga
      * griglia
      *-----------------------------------------------------------------
           initialize mdc-rec
           move tm-r-doc-tip (i)  to mdc-doc-tip
           move tm-r-doc-key (i)  to mdc-doc-key
           perform rd-clcmovdc
           if w-verbo-ok
              initialize t-gor-r-rec
              move tm-r-prog (i) to t-gor-r-prog
              perform rd-t-grid-det
              perform z-grid-det-riempi-campi-t-gor
              perform rwr-t-grid-det
           end-if
           .
      *----------------------------------------------------------------
      * Salvo gli aggiornamenti effettuati sulla tabella
      * Per il momento in questa fase aggiorno solo i campi variabili
      * modificabili dall'utente
      *----------------------------------------------------------------
       b-aggiorna.
           initialize t-gor-r-rec
           perform st-t-grid-det-notmin
           if w-verbo-ok
              perform with test before until w-fine-file
                 perform rdnxt-t-grid-det
                 if w-verbo-ok 

                    evaluate t-gor-r-riga-mod
                     when "S"
                       perform b-aggiorna-record
                    end-evaluate

                  else
                    move k-fine-file   to statusfi
                 end-if
              end-perform
           end-if
           move k-verbo-ok             to statusfi
           move "N"                    to fl-esc-agg
           .
      *----------------------------------------------------------------
      * Salvo gli aggiornamenti effettuati sulla tabella
      * Per il momento in questa fase aggiorno solo i campi variabili
      * modificabili dall'utente
      *----------------------------------------------------------------
       b-aggiorna-record.
           initialize mdc-rec
           move t-gor-r-doc-tip  to mdc-doc-tip
           move t-gor-r-doc-key  to mdc-doc-key
           perform rd-clcmovdc
           if w-verbo-ok
              move t-gor-r-note  to mdc-note
              if t-gor-r-data-cns = 99999999
                 move 0          to mdc-data-cns
               else
                 move t-gor-r-data-cns to mdc-data-cns
              end-if
              perform rwr-clcmovdc
           end-if
      * Azzero il flag di aggiornamento, dato che rimango nella griglia
           move " "              to t-gor-r-riga-mod
           perform rwr-t-grid-det
           .

      *---------------------------------------------------------------**
      * ENTRY POINT per aggiungere voci al menu pop-up per la griglia
      * GRID-DET
      *---------------------------------------------------------------**
       x-attiva-popup-grid-det-ep.
      *---------------------------------------------------------------**
      * ENTRY POINT per aggiungere voci al menu pop-up per la griglia
      * GRID-DET
      *---------------------------------------------------------------**
       z-grid-det-aggiungi-voci-popup-ep.

      *----------------------------------------------------------------
      * Creazione file di transito
      * Non esiste una chiave per data; quindi per ora scorro sempre
      * tutto
      *----------------------------------------------------------------
       z-grid-det-crea-transito.
           perform z-grid-det-crea-transito-init


           initialize mdc-rec
           if tm-stato-controllo = " "
              perform st-clcmovdc-notmin
            else
              move tm-stato-controllo   to mdc-stato-controllo
              perform st-clcmovdc-notmin-chia2
           end-if
           if w-verbo-ok
              perform with test before until w-fine-file
                 perform rdnxt-clcmovdc
                 if w-verbo-ok 

                    if tm-stato-controllo <> " "
                       if mdc-stato-controllo <> tm-stato-controllo
                          exit perform
                       end-if
                    end-if

                    perform z-grid-det-riempi-t-rec
                    if gor-r-fl-interrompi = "S"
                       exit perform
                    end-if
                    
                  else
                    move k-fine-file   to statusfi
                 end-if
              end-perform
           end-if
           move k-verbo-ok             to statusfi


           perform z-grid-det-crea-transito-fine
           .

       z-grid-det-riempi-t-rec.
           if mdc-di-data < tm-da-data or 
              mdc-di-data > tm-a-data
              exit paragraph
           end-if

           if tm-tipo <> "T"
              if mdc-doc-tip <> tm-tipo 
                 exit paragraph
              end-if
           end-if

           if tm-stato-controllo <> " "
              if tm-stato-controllo <> mdc-stato-controllo
                 exit paragraph
              end-if
           end-if

      * Righe collegate a movimento cancellato: per il momento le scarto
      * senza effettuare cancellazione
           move mdc-doc-key             to w-unpack-doc-key
           evaluate mdc-doc-tip
            when "E"
              initialize mag-recz
              move wud-e-riga           to mag-chiap
              perform rd-cogmomag
              if w-verbo-invalido
                 exit paragraph
              end-if
              move mag-merce3           to w-mdc-merce

              initialize mlt-rec
              move mag-merce3           to mlt-merce
              move mag-mag1             to mlt-mag
              move wud-e-prog           to mlt-prog
              perform rd-cogmerlt
              move mlt-lotto            to w-mdc-lotto
            when "L"
              initialize cla-rec
              move wud-l-ann            to cla-anno
              move wud-l-num            to cla-numero
              perform rd-coglavor
              move cla-merce            to w-mdc-merce
              string wud-l-ann "/" wud-l-num delimited size
                                      into w-mdc-lotto
           end-evaluate

           initialize t-gor-r-rec

           perform z-grid-det-riempi-campi-t-gor

           perform z-grid-det-inserisci-t-rec
           .

       z-grid-det-riempi-campi-t-gor.
           move mdc-doc-tip             to t-gor-r-doc-tip
           move mdc-doc-key             to t-gor-r-doc-key

           move mdc-stato-controllo     to t-gor-r-stato-cnt
           move mdc-qta-dec             to t-gor-r-qta-dec
           move mdc-qta-ok              to t-gor-r-qta-ok
           move mdc-qta-ko              to t-gor-r-qta-ko
      *    move mdc-qta-nc              to t-gor-r-qta-nc
           move mdc-di-data             to t-gor-r-data
           move mdc-stato-versa         to t-gor-r-fl-ela
           move mdc-linked-doc-tip      to t-gor-r-lnk-doc-tip
           move mdc-linked-doc-key      to t-gor-r-lnk-doc-key

           move mdc-note                to t-gor-r-note
           if mdc-data-cns not numeric
              move 0                    to mdc-data-cns
           end-if
           if mdc-data-cns = 0
              move 99999999             to t-gor-r-data-cns
            else
              move mdc-data-cns         to t-gor-r-data-cns
           end-if
      * Calcolo il numero di giorni da oggi alla data consegna
           perform x-calcola-t-gor-r-gg-cns
      *
           move w-mdc-merce             to t-gor-r-mer-cm
           move w-mdc-lotto             to t-gor-r-lotto
           perform z-riempi-t-gor-r-mer
           .

      * Determini i giorni consegna (utile per evidenziare le righe in
      * funzione del numero di giorni prima della consegna)
       x-calcola-t-gor-r-gg-cns.
           if t-gor-r-data-cns = 99999999
              move 99999999             to t-gor-r-gg-cns
            else
              initialize util-gesdate
              move "DIFFERENZA-DATE"    to gesdate-ope
              move t-gor-r-data-cns     to gesdate-data-in
              move datasis              to gesdate-data-cfr
              call "GESDATE"         using stringhe util-gesdate
              move gesdate-giorni       to t-gor-r-gg-cns
           end-if
           .

      *******************************************************************
      * Controlli sui campi non di dettaglio                            *
      *******************************************************************
       x-imposta-conferma.
           move "S"                      to x-f3-ok
           move spaces                   to x-f3-msg

           perform x-controlla-tm-tipo
           if x-f3-ok-parziale = "N"
              move "N"             to x-f3-ok
           end-if
           perform x-controlla-tm-da-data
           if x-f3-ok-parziale = "N"
              move "N"             to x-f3-ok
           end-if
           perform x-controlla-tm-a-data
           if x-f3-ok-parziale = "N"
              move "N"             to x-f3-ok
           end-if
           perform x-controlla-tm-stato
           if x-f3-ok-parziale = "N"
              move "N"             to x-f3-ok
           end-if
           .

      **---------------------------------------------------------------**
      ** Controllo campo: Magazzino
      **---------------------------------------------------------------**
       x-controlla-tm-tipo.
           move "S"                to x-f3-ok-parziale
 
 
           perform x-ctr-color-tm-tipo
           .
      **---------------------------------------------------------------**
      ** Controllo campo: Data Partenza
      **---------------------------------------------------------------**
       x-controlla-tm-da-data.
           move "S"                to x-f3-ok-parziale
 
           if tm-da-data = zeroes
              move "Data partenza obbligatoria" to x-f3-msg
              move "N"                   to x-f3-ok-parziale
           end-if
 
           perform x-ctr-color-tm-da-data
           .
      **---------------------------------------------------------------**
      ** Controllo campo: Data Arrivo
      **---------------------------------------------------------------**
       x-controlla-tm-a-data.
           move "S"                to x-f3-ok-parziale
 
           if tm-a-data = zeroes
              move "Data arrivo obbligatoria" to x-f3-msg
              move "N"                   to x-f3-ok-parziale
           else
              if tm-da-data > tm-a-data
                 move "Data partenza maggiore di data arrivo!!"
                                         to x-f3-msg
                 move "N"                to x-f3-ok-parziale
              end-if
           end-if
 
           perform x-ctr-color-tm-a-data
           .
      **---------------------------------------------------------------**
      ** Controllo campo: Stato Controllo
      **---------------------------------------------------------------**
       x-controlla-tm-stato.
           move "S"                to x-f3-ok-parziale
 
           evaluate tm-stato
              when "1"   move "NT"      to tm-stato-controllo
              when "2"   move "NC"      to tm-stato-controllo
              when "3"   move "DC"      to tm-stato-controllo
              when "4"   move "IC"      to tm-stato-controllo
              when "5"   move "CC"      to tm-stato-controllo
              when "6"   move "CN"      to tm-stato-controllo
              when other move spaces    to tm-stato-controllo
           end-evaluate
 
           perform x-ctr-color-tm-stato
           .

      *-----------------------------------------------------------------
      * Serie di controlli che servono per verificare l'eventuale       
      * spostamento su un altro controllo con il mouse                  
      *-----------------------------------------------------------------
       x-test-mouse.
           move "S"                      to x-spostamento-mouse
           evaluate control-id
      *----------------------------------------------------------------
      * Movimento nell'area filtri
      *----------------------------------------------------------------
            when  101 thru  199
              go to x-test-mouse-filtri
      *----------------------------------------------------------------
      * Movimento dalla griglia: posso tornare sui filtri
      *----------------------------------------------------------------
            when k-id-grid-det
              evaluate event-control-id
               when  101 thru  199
                 perform x-attiva-f3-conferma
                 go to x-test-mouse-filtri
               when k-id-grid-det
                 go to a-grid
               when other
                 move "N"          to x-spostamento-mouse
              end-evaluate
           end-evaluate
           if x-spostamento-mouse = "N"
              move control-id            to event-control-id
              go to x-test-mouse
           end-if
           .

      *----------------------------------------------------------------
      * Intercetto il movimento del mouse sulla sezione "filtri"
      *----------------------------------------------------------------
       x-test-mouse-filtri.
           evaluate event-control-id
            when k-id-tm-tipo
              go to a-tm-tipo
            when k-id-tm-da-data
              go to a-tm-da-data
            when k-id-tm-a-data
              go to a-tm-a-data
            when k-id-tm-stato
              go to a-tm-stato
            when other
              move "N"          to x-spostamento-mouse
              move control-id   to event-control-id
              go to x-test-mouse
           end-evaluate
           .
      *******************************************************************
      *Attivo e disattivo i t.f. utilizzabili sulla griglia di dettaglio*
      *******************************************************************
       x-attiva-tf-grid.
           perform x-attiva-f3-aggiorna
           perform x-attiva-sf4-griglia
           perform x-attiva-sf5-esporta-excel
           perform x-attiva-ctrl-a-aggiungi
           .
       x-disattiva-tf-grid.
           perform x-disattiva-f3-aggiorna
           perform x-disattiva-sf4-griglia
           perform x-disattiva-sf5-esporta-excel
           perform x-disattiva-ctrl-a-aggiungi
           .
      * Gestione dei tasti funzione variabili della griglia
       x-gestione-tf-grid-var.
      * Elaborazione: possibile solo sui movimenti completamente
      * controllati e non elaborati
      * Deve esserci anche una quantita' OK
      * NB: permetto anche di effettuare l'operazione nel caso in cui 
      *     non ho ancora ricevuto il controllo: in questo caso forzo
      *     tutta la quantita' come ok
           if (i > 0 and i <= tm-grid-det-dati-pnt) and
              tm-r-fl-ela (i) <> "S"                and
              tm-r-lnk-doc-key (i) = " "            and
             ((tm-r-qta-ok      (i) > 0  and
              tm-r-stato-cnt (i) = "CC"     ) or
              (tm-r-stato-cnt (i) = "IC"    )   )
              if w-pbc-visible (k-sf6-elabora) = 0
      * Limito questa operazione ad alcuni operatori
                 if wo-oper = 11 or
                    wo-oper = 12 or
                    wo-oper = 14 or
                    wo-oper = 29 or
                    wo-oper = 37 or
                    wo-oper = 777    
                    move k-sf6-elabora to w-pbc-idx
                    perform x-attiva-pbc
                 end-if
              end-if
            else
              if w-pbc-visible (k-sf6-elabora) = 1
                 move k-sf6-elabora to w-pbc-idx
                 perform x-disattiva-pbc
              end-if
           end-if
           .

      * Riempo il dato in griglia e il corrispondente dato editato sul
      * file
       x-riempi-t-gor-r-lnk-doc-key-ed.
           move " "                     to t-gor-r-lnk-doc-key-ed
           if t-gor-r-lnk-doc-tip = "E" or
              t-gor-r-lnk-doc-tip = "T"
              string t-gor-r-lnk-doc-key (1:1) "/"
                     t-gor-r-lnk-doc-key (2:4) "/"
                     t-gor-r-lnk-doc-key (6:1) "/"
                     t-gor-r-lnk-doc-key (7:6) 
                  delimited size        into t-gor-r-lnk-doc-key-ed
           end-if
           .

      * Data consegna: se e' "99999999" la mostro vuota
      *  (in questo modo preservo l'ordinamento corretto ma la
      *  visualizzazione e' chiara)
       x-riempi-t-gor-r-data-cns-ed.
           move " "                     to t-gor-r-data-cns-ed
           if t-gor-r-data-cns = 99999999
              move " "                  to t-gor-r-data-cns-ed
            else
              string t-gor-r-data-cns (7:2) "/"
                     t-gor-r-data-cns (5:2) "/"
                     t-gor-r-data-cns (1:4) 
                  delimited size        into t-gor-r-data-cns-ed
           end-if
           .

      *******************************************************************
      * Operazioni che devono essere eseguite dopo che nella grid viene *
      * eseguito un evento (o meglio, il cursore si sposta su una cella *
      * diversa)                                                        *
      **---------------------------------------------------------------**
      * OCCHIO all'utilizzo di FL-SONO-SU-GRID: vale "S" quando sono    *
      * gia` entrato nella grid, perche` se eseguo le istruzioni        *
      * presenti provenendo ad esempio da un controllo gestito con      *
      * COGS52 si inchioda tutto!!!                                     *
      *******************************************************************
       z-operazioni-acc-grid-det.
      *    evaluate event-type
      *-----------------------------------------------------------------
      * Questa operazione e' necessaria per gestire la griglia
      * completamente come read-only
      *-----------------------------------------------------------------
      *       when msg-begin-entry
      *          set event-action to event-action-fail-terminate
      *    end-evaluate
      *
           if fl-sono-su-grid = "S"
              if gor-r-n-m not = 0
                 perform z-carica-controlli
              end-if
           end-if
           perform x-gestione-tf-grid-var
           .

      **---------------------------------------------------------------**
      ** Valido se posso editare il contenuto di una colonna
      **---------------------------------------------------------------**
       z-99-begin-entry-grid-det.
           inquire e-grid-det,
             x            in w-cell-x,
             cell-data    in w-cell-data-sav,
             entry-reason in w-entry-reason
           evaluate w-cell-x
            when "xxx"
              continue
            when other
              set event-action   to event-action-fail-terminate
           end-evaluate
           .

      **---------------------------------------------------------------**
      ** Validazione accept su griglia e operazioni collegate
      **---------------------------------------------------------------**
       z-99-finish-entry-grid-det.
           inquire e-grid-det,
              x         in w-cell-x,
              y         in w-cell-y,
              cell-data in w-cell-data
           evaluate w-cell-x
            when "xxx"
              continue
           end-evaluate
           move i                         to gor-r-i
           perform z-modifica-riga-grid-det
           .
      *******************************************************************
      * Chiamata per la formattazione e la definizione delle colonne    *
      *   della griglia                                                 *
      *******************************************************************
       z-prepara-tm-grid.
           set handle-grid               to handle of e-grid-det.
           set handle-grid-appunti       to handle of e-grid-det.

           perform x-calcola-dimensione-cella

           inquire handle-maschera,
              lines in w-window-lines,
              size  in w-window-size

           perform x-ricalcola-grid-det
           perform x-ricalcola-frame

           modify e-grid-det, reset-grid = 1
           perform z-gridadmn-prepara-grid-det
           .

       z-gridadmn-prepara-grid-det.
      *******************************************************************
      * Qui si definiscono le personalizzazione alla griglia, dipendenti*
      * da configurazioni e/o personalizzazioni                         *
      *******************************************************************
           initialize gdad-custom
      *    if tm-cfg-acc-pma = "S"
      *       move "S"             to gdad-cst-vis (k-colid-pma)
      *       move "S"             to gdad-cst-vis (k-colid-curr-pma)
      *     else
      *       move "N"             to gdad-cst-vis (k-colid-pma)
      *       move "N"             to gdad-cst-vis (k-colid-curr-pma)
      *    end-if

           initialize util-gdad
           move "Prepara"                to gdad-ope
           move prog-err                 to gdad-prg
           move 1                        to gdad-frm
           move k-id-grid-det              to gdad-ctrl-id
           call "GRIDADMN" using stringhe util-gdad, null, gdad-custom
                                 gdad-hidden-data
           .

      *******************************************************************
      * Gestione eventi form                                            *
      *******************************************************************
       z-99-event-form.
           perform z-99-event-form-1
           .
       z-99-event-form-1.
           evaluate event-type
              when ntf-resized
              perform z-ntf-resized-on-grid-det
              perform x-ricalcola-frame
           end-evaluate
           .
       z-99-exception-form.
           perform z-cambia-controlli
           move k-exc-prosegui           to z-exception-prosegui
           if wesc
              perform test-wesc
              if wesc
                 move k-exc-esci         to z-exception-prosegui
                 exit paragraph
               else
                 move k-exc-rimani       to z-exception-prosegui
                 exit paragraph
              end-if
           end-if

      * Caricamento di un filtro memorizzato
           if ctrl-f
              perform z-imppredf-seleziona
              if imppredf-esito = "S"
                 perform x-imposta-conferma
                 display s-mm-1
                 perform z-carica-controlli
                 move k-exc-ricarica    to z-exception-prosegui
               else
                 move k-exc-rimani      to z-exception-prosegui
              end-if
           end-if
      * Salvataggio di un filtro memorizzato
           if ctrl-s
              perform z-imppredf-salva
              move k-exc-rimani         to z-exception-prosegui
           end-if
      *
      * Qui se i parametri di input sono diversi rispetto a quelli
      * caricati, chiedo di eseguire il salvataggio
           if tm-interfaccia <> tm-interfaccia-copia 
              if w-pbc-visible (k-ctrl-salva) = 0
                 move k-ctrl-salva         to w-pbc-idx
                 perform x-attiva-pbc
              end-if
            else
              if w-pbc-visible (k-ctrl-salva) = 1
                 move k-ctrl-salva         to w-pbc-idx
                 perform x-disattiva-pbc
              end-if
           end-if
           .
      *******************************************************************
      * Preparazione iniziale della screen;                             *
      * - Assegno i valori possibili alle COMBO                         *
      * - Assegno i valori possibili alle LIST-BOX                      *
      *******************************************************************
       z-costruisci-controlli.
           perform z-costruisci-s60
           .
      **-----------------------------------------------------------**
      ** Routine per assegnare un corretto valore ai controlli gestiti
      **   attraverso subroutine (per ora CHECK e RADIO)
      **-----------------------------------------------------------**
      ** Qui vengono gestiti anche i controlli particolari per
      **  abilitare/disabilitare i controlli
      **-----------------------------------------------------------**
       z-carica-controlli.
           perform z-cambia-controlli
           perform z-carica-vari
           .
       z-carica-vari.
           perform z-carica-s68
           perform z-carica-s95
           perform z-carica-s52
           perform z-carica-u10
           .


      *---------------------------------------------------------------**
      * Controllo campo: merce
      *---------------------------------------------------------------**
      * In questa modalita', se aggiorno il codice merce, devo anche
      * aggiornare sul file di transito i campi correlati
      *---------------------------------------------------------------**
       x-controlla-tm-r-mer.
           move "S"                to x-f3-ok-parziale

           if tm-r-mer-cm (i) <> grb-r-mer-cm
              initialize t-gor-r-rec
              move tm-r-riga (i)        to t-gor-r-prog
              perform rd-t-grid-det
              if w-verbo-ok
                 move tm-r-mer-cm (i)   to t-gor-r-mer-cm
                 perform z-riempi-t-gor-r-mer
                 perform rwr-t-grid-det
              end-if

           end-if


           move i                    to k-rg
           perform x-modify-riga-grid-det
           .

       x-controlla-tm-r-qta-ok.
           if tm-r-qta-ok (i) > tm-r-qta-dec (i)
              move "Quantita' troppo alta!!" to x-f3-msg
              perform x-set-f3-msg-grid-det
              move "N"                  to x-f3-ok-parziale
           end-if

           if x-f3-ok-parziale = "S"
              if tm-r-qta-ok (i) <> grb-r-qta-ok
                 perform z-aggiorna-t-gor-r-qta-ok
              end-if
           end-if
           .

      * I controlli sulle note servono solo per attivare l'aggiornamento
      * del transito
       x-controlla-tm-r-note.

      *    if x-f3-ok-parziale = "S"
      *       if tm-r-qta-ok (i) <> grb-r-qta-ok
      *          perform z-aggiorna-t-gor-r-qta-ok
      *       end-if
      *    end-if
           .
       x-controlla-tm-r-data-cns.
           initialize t-gor-r-rec
           move tm-r-prog (i)   to t-gor-r-prog
           perform rd-t-grid-det
           if w-verbo-ok
      * Se la data viene inserita a 0 ripristino 99999999
              if tm-r-data-cns (i) = 0
                 move 99999999          to tm-r-data-cns (i)
              end-if
              move tm-r-data-cns (i)    to t-gor-r-data-cns
              perform x-riempi-t-gor-r-data-cns-ed
      * Questo e' essenziale altrimenti il caricamento dei dati non e'
      * corretto: tenerne conto nel caso serva in altri programmi!!
              move t-gor-r-data-cns-ed  to tm-r-data-cns-ed (i)
              move "S"                  to t-gor-r-riga-mod
              perform x-calcola-t-gor-r-gg-cns
              perform rwr-t-grid-det
              move t-gor-r-gg-cns       to tm-r-gg-cns (i)
              move i                    to k-rg
              perform x-modify-riga-grid-det
           end-if
           .

      *---------------------------------------------------------------
      * Gestione eventi speciali sulla griglia GRID-DET
      *---------------------------------------------------------------
       z-grid-det-be-ep.
           evaluate event-type
      *---------------------------------------------------------------
      * Il right-click attiva il menu pop-up
      *---------------------------------------------------------------
            when msg-grid-rbutton-down
              call "W$MENU"          using wmenu-popup, 
                                           grid-det-menu-handle
                                    giving myResult
           end-evaluate
           .


       z-grid-det-mer-be-ep.
           move "N"             to w-cell-accept-ok
           .


       z-event-grid-det-ep.
      *---------------------------------------------------------------
      * Operazioni particolari da effettuare dopo che e' successo un
      * evento
      *---------------------------------------------------------------
       z-event-grid-det-after.
      *    evaluate event-type
      *     when msg-finish-entry
      *       evaluate w-cell-x
      *        when gcn-r-prezzo
      *          if num-fasce > 0
      *             if grb-r-prezzo not = tm-r-prezzo (i)
      *                perform lancia-cogs73
      *             end-if
      *          end-if
      *       end-evaluate
      *    end-evaluate
           .

       x-controlla-grid-det-col-ep.
       x-gridadmn-grid-det-ep.
       z-gestione-tf-grid-det-ep.
       z-usa-tf-grid-det-ep.
      *---------------------------------------------------------------
      * Entry point per chiamate a COGS65W
      *---------------------------------------------------------------
       z-s65-tm-r-ana-ep.
           move "C"                      to s65-i-tip
           move "N"                      to s65-fl-age
           move "N"                      to s65-fl-vet
           .
      *---------------------------------------------------------------
      * Entry point per chiamate a COGS68W
      *---------------------------------------------------------------
       z-s68-tm-r-mer-ep.
           move k-program-id             to s68-fun
           move titolo-maschera          to s68-fun-des
           move az-main-mag              to s68-i-mag
           .
       z-s68-t-gor-r-mer-ep.
           move k-program-id             to s68-fun
           move titolo-maschera          to s68-fun-des
           move az-main-mag              to s68-i-mag
           .
      *
       copy "grave.cpy".
       copy "mmmask.cpy".
       copy "opengen.cpy".
       copy "stato.cpy".
       copy "stato1.cpy".
       copy "winmsg.cpy".
      *
       copy "cogq54.prc".
       copy "cogq54.imppredf".

       copy "clcmovdc.k02".
      *----------------------------------------------------------------------
      *----------------------------------------------------------------------
      * INSERIRE IN SCREENS
      *----------------------------------------------------------------------
      *----------------------------------------------------------------------
      *
       end program.


