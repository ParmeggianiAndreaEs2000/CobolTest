       identification division.
       program-id.    win058.
       Author.        Andrea Parmeggiani - Eurosystem.
      *
      **--------------------------------------------------------------**
      ** window per ricerca clienti via descrizione
      ** Conversione Win: Gianluca 01/09/2005
      *  RIGA DI COMMENTO RELATIVA ALLA COMMESSA-TEST-2
      *  Aggiunto una riga nel titolo  
      **--------------------------------------------------------------**
       environment division.
       configuration section.
       source-computer. acu-cobol.
       object-computer. acu-cobol.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
       copy "feurtab.fd".
       copy "win058.select".
       copy "tran-cogsa1.fd".
       copy 'unanaso.fd'.
      *
           select cogtrans assign to random w-trs-name
             organization         is indexed
             access               is dynamic
             record key           is trs-chia1
             file status stato.
      *
       data division.
       file section.
      *
       fd  feurtab
           label record standard.
           copy "cogfiles.cpy".
       copy "win058.fd".
      *
       fd  unanaso
           label record standard.
           copy "unanaso.cpy".
      *
       fd  cogtrans
           label record standard.
       01  trs-rec.
           02 trs-chia1.
             03 trs-cod                 pic 9(05).
             03 trs-rif-key.
                04 trs-rif-tip          pic x(01).
                04 trs-rif-cod          pic 9(10).
             03 trs-dst.
                04 trs-dst-id-tip       pic x(01).
                04 trs-dst-id-cod       pic 9(04).
           02 trs-tip                   pic x(01).
           02 trs-val                   pic x(01).
           02 trs-cod-padre             pic 9(05).
           02 trs-desc                  pic x(40).
           02 trs-desc2                 pic x(40).
           02 trs-indi                  pic x(50).
           02 trs-cap                   pic x(07).
           02 trs-prov                  pic x(02).
           02 trs-loca                  pic x(40).
           02 trs-tel                   pic x(15).
           02 trs-sms                   pic x(15).
           02 trs-fax                   pic x(15).
           02 trs-piv                   pic 9(11).
           02 trs-cfi                   pic x(16).
           02 trs-email                 pic x(80).
           02 trs-web                   pic x(40).
           02 trs-tp-stt-des            pic x(40).
           02 trs-ana-chia.
              05 trs-ana-tipo           pic x(01).
              05 trs-ana-cod            pic 9(05).
           02 trs-rif-nome              pic x(50).
           02 trs-age-cod               pic 9(05).
           02 trs-old-cod               pic 9(06).
           02 trs-r-cod-iban.
              06 trs-app-iban-st     pic x(02).
              06 trs-app-iban-cin-e  pic x(02).
              06 trs-app-iban-cin-it pic x(01).
              06 trs-app-abi         pic z(05).
              06 trs-app-cab         pic z(05).
              06 trs-app-c-corr      pic x(12).
           02 trs-r-cod-rif          pic x(10).
      *
           02 trs-r-dst.
190117         05 trs-r-desc-dst         pic x(40).
190117         05 trs-r-indi-dst         pic x(40).
190117         05 trs-r-cap-dst          pic x(08).
190117         05 trs-r-loca-dst         pic x(30).
190117         05 trs-r-prov-dst         pic x(02).
      *
           02 trs-selezione              pic x(01).
           02 trs-dst-canc               pic x(01).
      *
       copy "tran-cogsa1.cpy".
      *
       working-storage section.
       copy "win058.wrk".
       copy "k-personal.cpy".
      *******************************************************************
      * Numero massimo di elementi della griglia, supponendo che sia    *
      * massimizzata                                                    *
      *******************************************************************
       78 k-max-ele-tab               value 100.
       78 k-sf8-ins-provv             value 01.
      *
       78  k-ricerca-and      value "T".
       78  k-ricerca-or       value "C".
       78  k-dove-tutti       value "T".
       78  k-dove-ana         value "A".
       78  k-dove-note        value "N".
       78  k-dove-nome        value "O".
       77  nf-trs-cogsa1-name      pic x(70).
      *
       77 w-t-size                      pic 9(03) value k-max-ele-tab.
       77 t-size                        pic 9(03).
       77 t-length                      pic 9(03) value k-max-ele-tab.
       77 n-m                           pic 9(04).
       77 fl-read                       pic x(01).
       77 i                             pic 9(04).
       77 j                             pic 9(04).
       77 i1                            pic 9(04).
       77 j1                            pic 9(04).
       77 si                            pic 9(04).
       77 sj                            pic 9(04).
       77 w-end-read                    pic x(01).
       77 nm                            pic 9(04).
       77 fl-fill                       pic x(01).
       77 w-caller                      pic x(08).
       01 w-t-key                       pic x(200).
       01 w-b-key                       pic x(200).
190117 01 w-fl-ok-dst                   pic x(01).
      *
       77 k                             pic 9(04).
       77 z                             pic 9(04).
      *
       77 w-trs-name                    pic x(70).
       77 fl-tapi                       pic x(01).
       77 n-l                           pic 9(03) comp-4.
       77 n-d                           pic 9(03) comp-4.
       77 n-i                           pic 9(03) comp-4.
      *77 w-i                           pic 9(03) comp-4.
      *77 w-d                           pic 9(03) comp-4.
      *77 w-s                           pic 9(03) comp-4.
       77 n-so                          pic 9(03) comp-4.
       77 w-min                         pic 9(03) comp-4.
       77 w-len                         pic 9(03) comp-4.
       77 w-str                         pic 9(03) comp-4.
       77 w-par                         pic 9(03) comp-4.
       77 w-old                         pic 9(03) comp-4.
       01 old-chia                      pic x(09) value spaces.
       01 old-ulti                      pic 9(08) comp-4.
       01 old-tabe                      pic 9(08) comp-4.
       01 flagerr                       pic 9(01) value zero.
       01 ctrt                          pic 9(04).
       01 ctrp                          pic 9(04).
      *01 ctrf                     pic 9(04).
      *01 ctrd                     pic 9(04).
      *01 ctrd1                    pic 9(04).
       01 prog                          pic 9(08).
       01 prog1                         pic 9(08).
       01 punt                          pic 9(08).
       01 ind                           pic 9(08).
       01 w-stringa-inp.
         02 wsi-ele occurs 40 indexed by i-si.
           03 wsi-pic                   pic x(01).
      *
       01 w-parola.
         02 wp-ele occurs 9 indexed by i-wp.
           03 wp-pic                    pic x(01).
      *
       01 w-parola1.
         02 wp1-ele occurs 9 indexed by i-wp1.
           03 wp1-pic                   pic x(01).
      *
       01 tab-l-stringhe.
         02 tm-l-ele occurs 0 to 10 depending on n-l
                  indexed by i-l.
           03 tm-l                      pic 9(01).
      *
       01 tab-d-stringhe.
         02 tm-d-ele occurs 0 to 20 depending on n-d
                  indexed by i-d.
           03 tm-d                      pic x(01).
      *
       01 tab-i-stringhe.
         02 tm-i-ele occurs 0 to 20 depending on n-i
                  indexed by i-i.
           03 tm-i                      pic x(01).
      *
       01 tab-stringhe-out.
         02 tso-ele occurs 0 to 100 depending on n-so
            indexed by i-so.
           03 tso-len                   pic 9(02) comp-4.
           03 tso-str.
             04 tso-ele occurs 30 indexed by j-so.
               05 tso-pic               pic x(01).
      *
       01 w-tabe-58.
           03 w58-t                     pic 9(04) comp-4.
           03 w58-paro occurs 0 to 3 depending on w58-t
              indexed by w58-i          pic x(09).
      *******************************************************************
      * Record di interscambio con griglia                              *
      *******************************************************************
       01 grid-record.
         02 grd-cod                     pic z(05).
         02 grd-desc                    pic x(40).
         02 grd-val                     pic x(02).
         02 grd-indi                    pic x(35).
         02 grd-cap                     pic x(07).
         02 grd-loca                    pic x(40).
         02 grd-prov                    pic x(02).
         02 grd-tel                     pic x(15).
         02 grd-fax                     pic x(15).
         02 grd-piv                     pic z(11).
         02 grd-cfi                     pic x(16).
         02 grd-email                   pic x(80).
         02 grd-web                     pic x(40).
         02 grd-cod-padre               pic z(05).
         02 grd-desc2                   pic x(40).
         02 grd-tp-stt-des              pic x(40).
         02 grd-rif-nome                pic x(50).
190117   02 grd-desc-dst                pic x(40).
190117   02 grd-indi-dst                pic x(40).
190117   02 grd-cap-dst                 pic x(08).
190117   02 grd-loca-dst                pic x(30).
190117   02 grd-prov-dst                pic x(02).
         02 grd-selezione               pic x(12).
         02 grd-dst                     pic x(05).
      *
       01 tab-maschera.
         02 tm-no-canc                  pic x(01).
         02 tm-fl-cli-for               pic x(01).
         02 tm-fl-contatti              pic x(01).
         02 tm-fl-destinazioni          pic x(01).
         02 tm-tab.
           03 tm-righe occurs k-max-ele-tab times.
             04 tm-ele.
               05 tm-r-tip              pic x(01).
               05 tm-r-cod              pic 9(05).
               05 tm-r-cod-padre        pic 9(05).
               05 tm-r-e-cod-padre redefines tm-r-cod-padre pic z(05).
               05 tm-r-desc             pic x(40).
               05 tm-r-desc2            pic x(40).
               05 tm-r-val              pic x(01).
               05 tm-r-indi             pic x(35).
               05 tm-r-cap              pic x(07).
               05 tm-r-prov             pic x(02).
               05 tm-r-loca             pic x(33).
               05 tm-r-loca-40          pic x(40).
               05 tm-r-sms              pic x(15).
               05 tm-r-sms-btn          pic x(15).
               05 tm-r-tel              pic x(15).
               05 tm-r-tel-btn          pic x(15).
               05 tm-r-fax              pic x(15).
               05 tm-r-piv              pic 9(11).
               05 tm-r-cfi              pic x(16).
               05 tm-r-email            pic x(80).
               05 tm-r-web              pic x(40).
               05 tm-tp-stt-des         pic x(40).
               05 tm-r-rif-key.
                  06 tm-r-rif-tip       pic x(01).
                  06 tm-r-rif-cod       pic 9(10).
               05 tm-r-rif-nome         pic x(50).
               05 tm-r-age-cod          pic 9(05).
               05 tm-r-age-des          pic x(40).
               05 tm-r-old-cod          pic 9(06).
               05 tm-r-cod-iban.
                  06 tm-app-iban-st     pic x(02).
                  06 tm-f1              pic x(01).
                  06 tm-app-iban-cin-e  pic x(02).
                  06 tm-f2              pic x(01).
                  06 tm-app-iban-cin-it pic x(01).
                  06 tm-f3              pic x(01).
                  06 tm-app-abi         pic z(05).
                  06 tm-f4              pic x(01).
                  06 tm-app-cab         pic z(05).
                  06 tm-f5              pic x(01).
                  06 tm-app-c-corr      pic x(12).
               05 tm-r-cod-rif          pic x(10).
190117         05 tm-r-desc-dst         pic x(40).
190117         05 tm-r-indi-dst         pic x(40).
190117         05 tm-r-cap-dst          pic x(08).
190117         05 tm-r-loca-dst         pic x(30).
190117         05 tm-r-prov-dst         pic x(02).
               05 tm-r-dst.
                  07 tm-r-dst-tip       pic x(01).
                  07 tm-r-dst-idx       pic 9(04).
               05 tm-r-selezione .
                  07 tm-r-selez1        pic x(01).
                  07 tm-r-selez2        pic x(11).
      *
       01 tab-mas-cfg.
         02 tm-cfg-upper                pic x(01).
         02 tm-cfg-ricerca-strif        pic x(01).
         02 tm-cfg-colore-riga-rif      pic 9(08).
         02 tm-cfg-abi-areasms          pic x(01).
         02 tm-cfg-no-canc              pic x(02).
         02 tm-cfg-destinazioni         pic x(01).
         02 tm-cfg-colore-riga-dst      pic 9(08).

       77   w-color                pic 9(08).
      *
       01  s-name-files.
           03 sf-num-files         pic 9(03)   value 1.
           03 sf-name-tab          pic x(70).
           03 sf-tab-files.
              05 sf-name-1         pic x(70).
              05 filler            pic x(03)   value '001'.
           03 sf-files redefines sf-tab-files.
            04 sf-ele occurs 1 indexed by ind-ls.
              05 sf-name           pic x(70).
              05 sf-num            pic 9(03).
      *
       01 datasis                       pic 9(08).
       01 filler redefines datasis.
         02 sis-aa                      pic 9(04).
         02 sis-mm                      pic 9(02).
         02 sis-gg                      pic 9(02).
      *
       01 w-anas-dadimis     pic 9(8).
       01 filler redefines w-anas-dadimis.
          05 w-anas-aadimis   pic 9(04).
          05 w-anas-mmdimis   pic 9(02).
          05 w-anas-ggdimis   pic 9(02).
      *
       01  w-filtro-tipologie.
           03 w-filtro-tipologia occurs 2 pic x(02).
      *
       77 m-num-parametri                pic 9(04) comp-1.
       77 m-presenza-w06-dati-aggiunti   pic x(01).
       77 timesis                        pic 9(08).
      *
       01 w-fl-esito-ragsoc             pic x(01) value spaces.
       01 w-fl-esito-indi               pic x(01) value spaces.
       01 w-fl-esito-loca               pic x(01) value spaces.
      *
       copy "wgrave.cpy".
       copy "wnscr.cpy".
       copy "wopenf.cpy".
       copy "wstato.cpy".
       copy "cogazien.cpy".
      *
       copy "wgrid.cpy".
       copy "utilgdad.cpy".
       copy "utilncnf.cpy".
       copy "utilxtapi.cpy".
       copy "utilbrws.cpy".
       copy "utilpost.cpy".
       copy "utilareasms.cpy".
       copy "utilcogsa1.cpy".
      *
       copy "utilcrm030.cpy".
       copy "utile08r.cpy".
       copy "utilwin058r.cpy".
       copy "utilf61.cpy".
       copy "utildlg.cpy".
       copy "utilgesstr.cpy".
      *
       linkage section.
       copy "wcont.cpy".
       copy "utilg32.cpy".
       copy "utilx47.cpy".
       copy "wcont1.cpy".
       copy "utilw06.cpy".
      *******************************************************************
      * Screen gestita dal programma                                    *
      *******************************************************************
       screen section.
       copy "win058-01.scr".
      *
       procedure division using stringhe util-w06
                                         util-w06-dati-aggiuntivi .
       copy "win058.decla".
            cogtrans tran-cogsa1
            .
       io-error-rout. exit.
       end declaratives.
       main section.
       apri.
           move k-program-id             to prog-err w-nome-hlp
           perform z-99-init-program
           initialize w-filtro-tipologie
           call "C$NARG"        using m-num-parametri
           move "N"                    to m-presenza-w06-dati-aggiunti
           evaluate m-num-parametri
            when 3
              move "S"                 to m-presenza-w06-dati-aggiunti
           end-evaluate

           if w06-out = 16  and az-personal =  k-personal-unione-ag-bo
              move "COSO16" to w-caller
            else
              move spaces   to w-caller
           end-if

           if w06-out-x-1 = "T"
                move w06-out-x-2  to w-filtro-tipologia (1)
                move w06-out-x-3  to w-filtro-tipologia (2)
                if w-filtro-tipologia (1) not = spaces and
                   w-filtro-tipologia (2) = spaces
                    move w-filtro-tipologia (1) to
                         w-filtro-tipologia (2)
                end-if
                move zero to w06-out
           end-if

           perform rd-azi
           perform open-files
           perform x-carica-lingua

           if az-personal = k-personal-unione-ag-bo
              perform open-files-s
           end-if.
           accept timesis from time
           move spaces                   to w-trs-name
           string ext-tmp-dir     delimited by "  "
                  ext-os-slash    delimited by size
                  "#$"            delimited by size
                  k-program-id    delimited by size
                  "-"
                  timesis
                  "."             delimited by size
                  wo-oper         delimited by size
                  into w-trs-name
           close cogtrans
           open output cogtrans
           if stato not = "00"
              perform x-set-m-win058-2-1-id
              move m-win058-2-1 to wb-1
              perform x-set-m-win058-12-1-id
              string m-win058-12-1   delimited by size
                     stato          delimited by size
                     into wb-2
              perform box-msg
              initialize w06-out
              go to fine
           end-if
           close cogtrans
           open i-o cogtrans
      *******************************************************************
      * Tasti funzione previsti nel programma                           *
      *******************************************************************
           move 1                        to w-presenza-invio-seleziona
           move 1                        to w-presenza-f1-configura
           move 1                        to w-presenza-sf4-griglia
           move 1                        to w-presenza-sf5-esporta-excel
           move 1                        to w-presenza-f6-scheda
           move 1                        to w-presenza-sf7-contatti
           move 1                        to w-presenza-f5-ricerche

      **---------------------------------------------------------------**
      * Caricamento bitmap custom: per  la routine load-bitmap-custom   *
      * devo indicare i nomi dei file delle bitmap e quante bitmap      *
      * custom carico (in w-pbc-idx-handle)                             *
      **---------------------------------------------------------------**
           initialize w-push-button-custom
           move 1                        to w-pbc-idx-handle
           move "anagr"                  to w-pbc-bitmap-file (01)
           perform load-bitmap-custom
      **---------------------------------------------------------------**
      * Crazione pulsanti custom: w-pbc-idx-handle mi dice da quale     *
      * file prendere il pulsante; w-pbc-bitmap number mi dice quale    *
      * bitmap prendere all'interno del file.                           *
      **---------------------------------------------------------------**
           move 1                        to w-pbc-idx-handle
           move 8                        to w-pbc-bitmap-number
           move m-win058-13-1 to w-pbc-title
           move k-sf8                    to w-pbc-exception
           perform x-display-push-button-custom

           accept datasis              from century-date
           initialize util-ncnf
           perform x99-src-cfg
           perform x999-src-cfg

           if tm-cfg-upper = "S"
              perform a-uppercase
           end-if

           initialize euta-rec
           move 70                       to euta-tipo
           move "RICE"                   to euta-cfg-arg
           move "RICANA"                 to euta-cfg-key
           perform rd-cogtabel
           if not w-verbo-ok
              move all "*"               to euta-descr
              initialize euta-c010
           end-if
           perform d-load-cfg
           perform src-len
           move i-si                     to w-len
           perform b-esplodi
           perform c-stringhe
      *
           perform x-set-m-win058-3-1-id
           move m-win058-3-1 to titolo-maschera
           move k-m-v-size               to m-v-size
           move k-m-h-size               to m-h-size
           perform rd-msk-pop
           move 1                        to i
           display s-mm-1
           perform z-costruisci-controlli
           perform z-prepara-tm-grid

           move k-no                     to fl-tapi
           initialize x-tp-link
           move "check-config"           to xtl-ope
           call "XTAPI01" using stringhe x-tp-link
           cancel "XTAPI01"
           if xtl-ret-cod = spaces
              move k-yes                 to fl-tapi
           end-if
           perform x-set-m-win058-4-1-id
           move m-win058-4-1 to wb-msg
           perform clock-msg
           perform creo-file           thru ex-creo-file
           if (tm-cfg-ricerca-strif = k-yes)
              perform z-cogsa1-ricerca
           end-if
           if tm-cfg-destinazioni = k-no
              move "Salve" to wb-msg
              perform vbx-msg
              perform z-ricerca-cogdesti  thru ex-ricerca-cogdesti
           end-if

           perform rem-clk
           .
       a4.
           modify e-tm-tab, mass-update = 1
           move 1                        to i
           perform z-svuota-tutto-grid
           modify e-tm-tab, mass-update = 0

           perform x-status-bar-init
           move zero                     to n-m
           initialize tab-maschera
           move tm-cfg-no-canc           to tm-no-canc
           move "S"                      to tm-fl-cli-for
           move tm-cfg-ricerca-strif     to tm-fl-contatti
           move tm-cfg-destinazioni      to tm-fl-destinazioni

           perform z-carica-controlli
           perform z-costruisci-grid
           move 1                        to i
           display s-mm-1
           .
       a-grid-ini.
           perform carica-tab          thru ex-carica-tab
           if n-m = 0

011015**--------------------------------------------------------------**

              initialize util-dlg
      *       move "Ricerca Anagrafica Per Descrizione"   to dlg-tit
      *       string "Nessuna anagrafica trovata per "
      *              "parametri di ricerca indicati"
      *                  delimited by size into dlg-msg
              perform x-set-m-win058-3-1-id
              move m-win058-3-1        to dlg-tit

              string m-nessuna-anagrafica-trovata
                     delimited by size into dlg-msg

              add 1                 to dlg-pnt
              move k-fun-wesc       to dlg-ope-fun-value (dlg-pnt)
              move "ESC"            to dlg-ope-fun (dlg-pnt)
              move m-esci           to dlg-ope-text (dlg-pnt)
              add 1                 to dlg-pnt
              move k-f5             to dlg-ope-fun-value (dlg-pnt)
              move "F5"             to dlg-ope-fun       (dlg-pnt)
      *       move "Ricerca con parametri " to dlg-ope-text (dlg-pnt)
              move m-ricerca-con-parametri  to dlg-ope-text (dlg-pnt)
              if (tm-no-canc = k-yes)
                 add 1                  to dlg-pnt
                 move k-f7              to dlg-ope-fun-value (dlg-pnt)
                 move "F7"              to dlg-ope-fun       (dlg-pnt)
      *          move "Ricerca anche cancellati "
                 move m-ricerca-anche-cancellati
                                        to dlg-ope-text (dlg-pnt)
              end-if

              call "DIALOGS"      using stringhe util-dlg
              cancel "DIALOGS"
              move dlg-exception    to funzio

              if wesc
                 go to a-grid-fine
              end-if

              if f7 and
                 (tm-no-canc = k-yes)
                  move zero                     to n-m
                  initialize tab-maschera
                  move "N"              to tm-no-canc
                  move "S"                      to tm-fl-cli-for
                  move tm-cfg-ricerca-strif     to tm-fl-contatti
                  move tm-cfg-destinazioni      to tm-fl-destinazioni
                  perform z-carica-controlli
                  move 1                        to i
                  display s-mm-1
                  go to a-grid-ini
              end-if

              if f5
                 move w06-in-tipo     to e08r-i-tip
                 call "COGE08R" using stringhe util-e08r
                 cancel "COGE08R"
      *          move e08r-exception     to funzio

                 if e08r-o-num = zero and
                    w06-in-tipo = "F"
                    perform x-set-m-win058-5-1-id
                    move
                    m-win058-5-1
                                         to wb-msg
                    perform vbx-msg-error
                    go to a-grid-fine
                 end-if

                 if e08r-o-num = zero and
                    w06-in-tipo = "C"

                    initialize util-dlg
                    perform x-set-m-win058-3-1-id
                    move m-win058-3-1     to dlg-tit

      *             string "Nessuna anagrafica trovata per "
      *              "parametri di ricerca indicati"
                    string m-nessuna-anagrafica-trovata
                     delimited by size into dlg-msg
                    add 1                 to dlg-pnt
                    move k-fun-wesc       to dlg-ope-fun-value (dlg-pnt)
                    move "ESC"            to dlg-ope-fun (dlg-pnt)
                    move m-esci           to dlg-ope-text (dlg-pnt)

                    add 1                 to dlg-pnt
                    move k-sf8            to dlg-ope-fun-value (dlg-pnt)
                    move "Shift-F8"       to dlg-ope-fun       (dlg-pnt)
      *             move "Inserisci Cliente Provvisorio "
                    move m-inserisci-cli-prov
                                    to dlg-ope-text (dlg-pnt)


                    call "DIALOGS"      using stringhe util-dlg
                    cancel "DIALOGS"
                    move dlg-exception    to funzio

                    if wesc
                       go to a-grid-fine
                    end-if

                    if s-f8  and
                       w06-in-tipo = "C"
                       initialize util-x47
                       move "COGX47-CALLED"         to x47-called
                       move k-program-id            to x47-caller
                       move w06-in-tipo             to x47-ana-tipo
                       move zero                    to x47-ana-cod
                       call "COGX47" using stringhe
                       cancel "COGX47"
                       if x47-ana-cod not = zero
                          move x47-ana-cod          to w06-out
                          go to fine
                       end-if
                       go to a-grid-fine
                    end-if
                 end-if

                 initialize util-w058r
                 move w06-in-tipo        to w058r-in-tipo
                 move zero               to w058r-out
                 call "WIN058R" using stringhe util-w058r
                 cancel "WIN058R"

                 if w058r-out not = zero
                    move w058r-out          to w06-out
                    go to fine
                 end-if
                 if (tm-no-canc = k-yes)
                    go to a-tm-no-canc
                 end-if

                 go to a-grid-fine
              end-if
011015**--------------------------------------------------------------**

              go to a-grid-fine
           end-if
           .
       a-grid.
           perform x-attiva-tf-grid
           modify e-tm-tab, enabled = 1
           perform z-99-accept-paged-grid

           if f-event
              if event-type = msg-begin-entry
                 inquire e-tm-tab,
                   entry-reason in w-entry-reason
                 if w-entry-double-click or
                    w-entry-enter
                    set funzio           to k-fun-invio
                 end-if
               else
                 go to x-test-mouse
              end-if
           end-if
           if wesc
              go to a-grid-fine
           end-if

           if f1
              initialize util-ncnf
              move "Modifica"            to ncnf-ope
              perform x99-src-cfg
              go to a-grid
           end-if
      *
           if s-f4
              initialize util-gdad
              move k-program-id          to gdad-prg
              move titolo-maschera       to gdad-prg-des
              move 1                     to gdad-frm
              move k-id-grid             to gdad-ctrl-id
              call "GRIDADMN" using stringhe util-gdad
              go to a-grid
           end-if
      *
           if s-f5
              perform z-esporta-paged-grid
              go to a-grid-ini
           end-if
      *
           if pg-dwn
              if fl-read = "E"
                 perform x-set-m-win058-6-1-id
                 move m-win058-6-1 to wb-msg
                 perform box-msg
                 go a-grid
              else
                 perform pg-after-pg-dwn
                 go to a-grid
              end-if
           end-if
      *
           if pg-up
              if fl-read = "B"
                 perform x-set-m-win058-7-1-id
                 move m-win058-7-1 to wb-msg
                 perform box-msg
                 go to a-grid
              else
                 perform pg-after-pg-up
                 go to a-grid
              end-if
           end-if

           if invio
              move tm-r-cod (i)          to w06-out
              if m-presenza-w06-dati-aggiunti = "S"
                 move tm-r-dst (i)       to w06-o-dst
              end-if
              go to fine
           end-if

           if f6
              if w06-in-tipo = "C"
                 if az-cli-pot = 'S'
                    initialize util-crm030
                    move "CRM030-CALLED"    to crm030-called
                    move tm-r-tip (i)       to crm030-ana-tip
                    move tm-r-cod (i)       to crm030-ana-cod
                    call "CRM030" using stringhe util-crm030
                    cancel "CRM030"
                  else
                    initialize util-f61
                    move tm-r-tip (i)          to f61-tipo
                    move tm-r-cod (i)          to f61-cod
                    call "COGF61" using stringhe util-f61
                    cancel "COGF61"
                 end-if
              else
                 if az-cli-pot = 'S'
                    initialize util-crm030
                    move "CRM030-CALLED"    to crm030-called
                    move tm-r-tip (i)       to crm030-ana-tip
                    move tm-r-cod (i)       to crm030-ana-cod
                    call "CRM030" using stringhe util-crm030
                    cancel "CRM030"
                 end-if
              end-if
              go to a-grid
           end-if

           if s-f6 and
              tm-r-web (i) not = spaces
              perform s3-web           thru ex-s3
              go to a-grid
           end-if

           if f7 and
              tm-r-email (i) not = spaces
              perform s2-mail          thru ex-s2
              go to a-grid
           end-if

           if s-f7
              initialize util-g32
              move "remote-win"          to g32-fun
              move k-program-id          to g32-caller
              move "COGG32"              to g32-called
              move tm-r-tip (i)          to g32-ana-tip
              move tm-r-cod (i)          to g32-ana-cod
              call "COGG32" using stringhe
              cancel "COGG32"
           end-if

           if f9 and
              fl-tapi = k-yes and
              tm-r-tel (i) not = spaces
              initialize x-tp-link
              move tm-r-tel (i)          to xtl-numero
              move "chiama"              to xtl-ope
              call "XTAPI01" using stringhe x-tp-link
              cancel "XTAPI01"
              go to a-grid
           end-if

           if f8 and
              x-enabled-tm-r-sms-btn = 1 and
              tm-r-sms (i)          <> spaces
              initialize util-areasms
              move tm-r-sms (i)          to areasms-dest-num
              move "INVIA"               to areasms-ope
              call "AREASMS"          using stringhe util-areasms
              cancel "AREASMS"
              go to a-grid
           end-if

011015**--------------------------------------------------------------**
           if f5
              move w06-in-tipo     to e08r-i-tip
              call "COGE08R" using stringhe util-e08r
              cancel "COGE08R"
      *       move e08r-exception     to funzio

                 if e08r-o-num = zero and
                    w06-in-tipo = "F"
                    perform x-set-m-win058-8-1-id
                    move
                    m-win058-8-1
                                         to wb-msg
                    perform vbx-msg-error
                    go to a-grid
                 end-if

                 if e08r-o-num = zero and
                    w06-in-tipo = "C"


                    initialize util-dlg
                    perform x-set-m-win058-3-1-id
                    move m-win058-3-1     to dlg-tit
                    string m-nessuna-anagrafica-trovata
                     delimited by size into dlg-msg

                    add 1                 to dlg-pnt
                    move k-fun-wesc       to dlg-ope-fun-value (dlg-pnt)
                    move "ESC"            to dlg-ope-fun (dlg-pnt)
                    move m-esci           to dlg-ope-text (dlg-pnt)

                    add 1                 to dlg-pnt
                    move k-sf8            to dlg-ope-fun-value (dlg-pnt)
                    move "Shift-F8"       to dlg-ope-fun       (dlg-pnt)
      *             move "Inserisci Cliente Provvisorio "
                    move m-inserisci-cli-prov
                                          to dlg-ope-text (dlg-pnt)
                    call "DIALOGS"      using stringhe util-dlg
                    cancel "DIALOGS"
                    move dlg-exception    to funzio

                    if wesc
                       go to a-grid
                    end-if

                    if s-f8  and
                       w06-in-tipo = "C"
                       initialize util-x47
                       move "COGX47-CALLED"         to x47-called
                       move k-program-id            to x47-caller
                       move w06-in-tipo             to x47-ana-tipo
                       move zero                    to x47-ana-cod
                       call "COGX47" using stringhe
                       cancel "COGX47"
                       if x47-ana-cod not = zero
                          move x47-ana-cod          to w06-out
                          go to fine
                       end-if
                       go to a-grid
                    end-if
                 end-if

              initialize util-w058r
              move w06-in-tipo        to w058r-in-tipo
              move zero               to w058r-out
              call "WIN058R" using stringhe util-w058r
              cancel "WIN058R"

              if w058r-out not = zero
                 move w058r-out          to w06-out
                 go to fine
              end-if

              go to a-grid
           end-if
011015**--------------------------------------------------------------**
           if s-f8  and
              w06-in-tipo = "C"
              initialize util-x47
              move "COGX47-CALLED"         to x47-called
              move k-program-id            to x47-caller
              move w06-in-tipo             to x47-ana-tipo
              move zero                    to x47-ana-cod
              call "COGX47" using stringhe
              cancel "COGX47"
              if x47-ana-cod not = zero
                 move x47-ana-cod          to w06-out
                 go to fine
              end-if

              go to a-grid
           end-if
011015**--------------------------------------------------------------**

           go to a-grid
           .
       a-grid-fine.
           perform x-disattiva-tf-grid
           initialize w06-out
           go to fine
           .
      **---------------------------------------------------------------**
      ** Cambio la selezione "Escludi Cancellati"
      **---------------------------------------------------------------**
       a-tm-no-canc.
           initialize util-s95
           perform z-s95-tm-no-canc
           if (wesc)
              go to fine
           end-if
           if f-event
              and (event-type = cmd-goto)
              go to x-test-mouse
           end-if
           go to a-grid-ini
           .
      **---------------------------------------------------------------**
      ** Cambio la selezione "clienti forntori "
      **---------------------------------------------------------------**
       a-tm-fl-cli-for.
           initialize util-s95
           perform z-s95-tm-fl-cli-for
           if (wesc)
              go to fine
           end-if
           if f-event
              and (event-type = cmd-goto)
              go to x-test-mouse
           end-if
           go to a-grid-ini
           .
      **---------------------------------------------------------------**
      ** Cambio la selezione "contatti "
      **---------------------------------------------------------------**
       a-tm-fl-contatti.
           initialize util-s95
           perform z-s95-tm-fl-contatti
           if (wesc)
              go to fine
           end-if
           if f-event
              and (event-type = cmd-goto)
              go to x-test-mouse
           end-if
           go to a-grid-ini
           .
      **---------------------------------------------------------------**
      ** Cambio la selezione "destinazioni"
      **---------------------------------------------------------------**
       a-tm-fl-destinazioni.
           initialize util-s95
           perform z-s95-tm-fl-destinazioni
           if (wesc)
              go to fine
           end-if
           if f-event
              and (event-type = cmd-goto)
              go to x-test-mouse
           end-if
           go to a-grid-ini
           .

      *
       fine.
       z-chiudi.
           close window w-sv-cur-prg
           perform z-close-files
           if az-personal = k-personal-unione-ag-bo
              close unanaso
           end-if.
           close cogtrans
           delete file cogtrans
           if (nf-trs-cogsa1-name <> spaces)
              and (tm-cfg-ricerca-strif = k-yes)
              close tran-cogsa1
              delete file tran-cogsa1
           end-if
           perform z-99-exit-program
           exit program
           .
      **---------------------------------------------------------------**
      ** ricerca stringhe contatti
      **---------------------------------------------------------------**
       z-cogsa1-ricerca.
           close tran-cogsa1
           initialize nf-trs-cogsa1-name
           initialize util-cogsa1
           move 'R'                to cogsa1-ope
           move 1                  to cogsa1-pnt
           move k-dove-nome        to cogsa1-dove  (cogsa1-pnt)
           move w06-in-str         to cogsa1-i-des (cogsa1-pnt)
           move k-no               to cogsa1-fl-log-msg
           move tm-cfg-upper       to cogsa1-cfg-upper
           move w06-in-tipo        to cogsa1-filter-rif-a-tip
           call "COGSA1"    using stringhe util-cogsa1
           cancel "COGSA1"
           if (cogsa1-esito = k-yes)
              and (cogsa1-nf-trs <> spaces)
              move cogsa1-nf-trs   to nf-trs-cogsa1-name
              move "I"             to fl-opr
              perform open-tran-cogsa1
              if (not w-verbo-ok)
                 exit paragraph
              end-if
              move low-values      to trs-cogsa1-chia1
              perform st-tran-cogsa1-notmin
              if (w-verbo-invalido)
                 exit paragraph
              end-if
              perform rdnxt-tran-cogsa1
              perform until (w-fine-file)
                 initialize trs-rec
                 move trs-cogsa1-rif-ana-tip  to ana-tipo
                                                 trs-tip
                                                 app-tipo
                 move trs-cogsa1-rif-ana-cod  to ana-cod
                                                 trs-cod
                                                 app-cod
                 perform rd-coggeana
                 if (w-verbo-invalido)
                    move all "*"            to ana-rec
                 end-if
                 perform fill-trs-sub-coggeana
                 move trs-cogsa1-rif-key    to trs-rif-key
                 move trs-cogsa1-rif-nome   to trs-rif-nome
                 if (trs-cogsa1-rif-tel  (1) <> spaces)
                    move trs-cogsa1-rif-tel  (1)  to trs-tel
                 end-if
                 if (trs-cogsa1-rif-fax <> spaces)
                    move trs-cogsa1-rif-fax       to trs-fax
                 end-if
                 if (trs-cogsa1-rif-mail <> spaces)
                    move trs-cogsa1-rif-mail      to trs-email
                 end-if
      *-----------------------------------------------------------------
      * Ricerco un numero valido per la spedizione SMS
      *-----------------------------------------------------------------
                 move spaces                    to trs-sms
                 if trs-cogsa1-rif-tel (1) (1:1) numeric and
                    trs-cogsa1-rif-tel (1) (1:1) > 0
                    move trs-cogsa1-rif-tel (1)   to trs-sms
                  else
                 if trs-cogsa1-rif-tel (2) (1:1) numeric and
                    trs-cogsa1-rif-tel (2) (1:1) > 0
                    move trs-cogsa1-rif-tel (2)   to trs-sms
                  else
                 if trs-cogsa1-rif-tel (3) (1:1) numeric and
                    trs-cogsa1-rif-tel (3) (1:1) > 0
                    move trs-cogsa1-rif-tel (3)   to trs-sms
                 end-if
                 end-if
                 end-if
                 move "O" to trs-selezione
                 perform wr-cogtrans
                 perform rdnxt-tran-cogsa1
              end-perform
           end-if
           .
      *******************************************************************
      * Serie di controlli che servono per verificare l'eventuale       *
      * spostamento su un altro controllo con il mouse                  *
      *******************************************************************
       x-test-mouse.
           move "S"                      to x-spostamento-mouse
           evaluate control-id
               when k-id-tm-no-canc
                 go to a-tm-no-canc
               when k-id-tm-fl-cli-for
                 go to a-tm-fl-cli-for
               when k-id-tm-fl-contatti
                 go to a-tm-fl-contatti
               when k-id-tm-fl-destinazioni
                 go to a-tm-fl-destinazioni
      *******************************************************************
      * Se sono su un altro campo escludo i casi precedenti             *
      *******************************************************************
              when k-id-grid
                 evaluate event-control-id
                    when k-id-tm-no-canc
                      go to a-tm-no-canc
                    when k-id-tm-fl-cli-for
                      go to a-tm-fl-cli-for
                    when k-id-tm-fl-contatti
                      go to a-tm-fl-contatti
                    when k-id-tm-fl-destinazioni
                      go to a-tm-fl-destinazioni
                    when k-id-grid
                       go to a-grid
                    when other
                       move "N"          to x-spostamento-mouse
                 end-evaluate
              when other
                 move "N"                to x-spostamento-mouse
           end-evaluate
           if x-spostamento-mouse = "N"
              move control-id            to event-control-id
              go to x-test-mouse
           end-if
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
              perform z-ntf-resized-on-grid-paged
           end-evaluate
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
       z-operazioni-accessorie-grid.
           evaluate event-type
              when msg-begin-entry
                 set event-action        to event-action-fail-terminate
           end-evaluate
           if fl-sono-su-grid = "S"
              if n-m not = 0
                 display e-dettaglio
                 perform z-carica-vari
                 perform z-cambia-controlli
              else
                 perform svuota-dettaglio
              end-if
           else
              perform svuota-dettaglio
           end-if
           .
       svuota-dettaglio.
      *    modify e-tm-r-info, value = " "
           .
      *******************************************************************
      * Chiamata per la formattazione e la definizione delle colonne    *
      *   della griglia                                                 *
      *******************************************************************
       z-prepara-tm-grid.
           set handle-grid               to handle of e-tm-tab.
           set handle-grid-appunti       to handle of e-tm-tab.

           perform x-calcola-dimensione-cella

           inquire handle-maschera,
              lines in w-window-lines,
              size  in w-window-size

           perform x-ricalcola-griglia

           modify e-tm-tab, reset-grid = 1
           perform z-gridadmn-prepara
           .
      *******************************************************************
      * Qui si definiscono le personalizzazione alla griglia, dipendenti*
      * da configurazioni e/o personalizzazioni                         *
      *******************************************************************
       z-gridadmn-prepara.
           initialize gdad-custom

           initialize util-gdad
           move "Prepara"                to gdad-ope
           move k-program-id             to gdad-prg
           move 1                        to gdad-frm
           move k-id-grid                to gdad-ctrl-id
           call "GRIDADMN" using stringhe util-gdad, null, gdad-custom
           .
      *******************************************************************
      * Riempimento della griglia                                       *
      *******************************************************************
       z-costruisci-grid.
           modify e-tm-tab, mass-update = 1

           perform z-svuota-tutto-grid

           move zero                     to k
           perform n-m times
              add 1                      to k
              perform x-riempi-grid-record
              move grid-record           to gdad-record

              perform x-add-record-to-grid
           end-perform
           modify e-tm-tab, num-rows = n-m + 1
           modify e-tm-tab, mass-update = 0
           .
       x-add-record-to-grid.
           initialize util-gdad
           move "Add-Record"             to gdad-ope
           move k-program-id             to gdad-prg
           move 1                        to gdad-frm
           move k-id-grid                to gdad-ctrl-id
           move k                        to i-grid-color
           perform x-set-row-grid-color
           move row-grid-color           to gdad-color
           call "GRIDADMN"            using stringhe util-gdad
                                            gdad-record
           .
      *
       z-svuota-tutto-grid.
           initialize util-gdad
           move "Svuota"                 to gdad-ope
           move k-program-id             to gdad-prg
           move 1                        to gdad-frm
           move k-id-grid                to gdad-ctrl-id
           call "GRIDADMN" using stringhe util-gdad gdad-record
           modify e-tm-tab, num-rows = 1
           perform svuota-dettaglio
           .
      *******************************************************************
      * Preparazione iniziale della screen;                             *
      * - Assegno i valori possibili alle COMBO                         *
      * - Assegno i valori possibili alle LIST-BOX                      *
      *******************************************************************
       z-costruisci-controlli.
           perform z-costruisci-s60
           .
      *******************************************************************
      * Routine per assegnare un corretto valore ai controlli gestiti   *
      * attraverso subroutine (per ora CHECK e RADIO)                   *
      *******************************************************************
      * Qui vengono gestiti anche i controlli particolari per           *
      *  abilitare/disabilitare i controlli                             *
      *******************************************************************
       z-carica-controlli.
           perform z-cambia-controlli
           perform z-carica-vari
           .
       z-carica-vari.
           perform z-carica-s52
           perform z-carica-s68
           perform z-carica-s93
           perform z-carica-s95
           perform z-carica-u10
           .
      *******************************************************************
      * gestione delle abilitazioni                                     *
      *******************************************************************
       z-cambia-tm-r-web.
           if i not > n-m
              if tm-r-web (i) not = spaces
                 if x-visible-tm-r-web = 0
                    move 1               to x-visible-tm-r-web
                    modify e-tm-r-web,
                           visible = x-visible-tm-r-web
                    modify e-label-tm-r-web,
                           visible = x-visible-tm-r-web
                 end-if
              else
                 if x-visible-tm-r-web = 1
                    move zero            to x-visible-tm-r-web
                    modify e-tm-r-web,
                           visible = x-visible-tm-r-web
                    modify e-label-tm-r-web,
                           visible = x-visible-tm-r-web
                 end-if
              end-if
           else
              if x-visible-tm-r-web = 1
                 move zero               to x-visible-tm-r-web
                 modify e-tm-r-web,
                        visible = x-visible-tm-r-web
                 modify e-label-tm-r-web,
                        visible = x-visible-tm-r-web
              end-if
           end-if
           .
       z-cambia-tm-r-email.
           if i not > n-m
              if tm-r-email (i) not = spaces
                 if x-visible-tm-r-email = 0
                    move 1               to x-visible-tm-r-email
                    modify e-tm-r-email,
                           visible = x-visible-tm-r-email
                    modify e-label-tm-r-email,
                           visible = x-visible-tm-r-email
                 end-if
              else
                 if x-visible-tm-r-email = 1
                    move zero            to x-visible-tm-r-email
                    modify e-tm-r-email,
                           visible = x-visible-tm-r-email
                    modify e-label-tm-r-email,
                           visible = x-visible-tm-r-email
                 end-if
              end-if
           else
              if x-visible-tm-r-email = 1
                 move zero               to x-visible-tm-r-email
                 modify e-tm-r-email,
                        visible = x-visible-tm-r-email
                 modify e-label-tm-r-email,
                        visible = x-visible-tm-r-email
              end-if
           end-if
           .
       z-cambia-tm-r-tel-btn.
           if fl-tapi = "S"
              if i not > n-m
                 if tm-r-tel-btn (i) not = spaces
                    if x-visible-tm-r-tel-btn = 0
                       move 1            to x-visible-tm-r-tel-btn
                       modify e-tm-r-tel-btn,
                              visible = x-visible-tm-r-tel-btn
                       modify e-label-tm-r-tel-btn,
                              visible = x-visible-tm-r-tel-btn
                    end-if
                 else
                    if x-visible-tm-r-tel-btn = 1
                       move zero         to x-visible-tm-r-tel-btn
                       modify e-tm-r-tel-btn,
                              visible = x-visible-tm-r-tel-btn
                       modify e-label-tm-r-tel-btn,
                              visible = x-visible-tm-r-tel-btn
                    end-if
                 end-if
              else
                 if x-visible-tm-r-tel-btn = 1
                    move zero            to x-visible-tm-r-tel-btn
                    modify e-tm-r-tel-btn,
                           visible = x-visible-tm-r-tel-btn
                    modify e-label-tm-r-tel-btn,
                           visible = x-visible-tm-r-tel-btn
                 end-if
              end-if
           else
              if x-visible-tm-r-tel-btn = 1
                 move zero               to x-visible-tm-r-tel-btn
                 modify e-tm-r-tel-btn,
                        visible = x-visible-tm-r-tel-btn
                 modify e-label-tm-r-tel-btn,
                        visible = x-visible-tm-r-tel-btn
              end-if
           end-if
           .
      *******************************************************************
      * Parametri di restart per chiave non minore                      *
      *******************************************************************
       rn-start-not-less.
           perform st-cogtrans-notmin
           if not w-verbo-ok
              move w-end-read            to fl-read
           end-if
           .
      *******************************************************************
      * Parametri di restart in avanti                                  *
      *******************************************************************
       rn-start.
           perform st-cogtrans-may
           if not w-verbo-ok
              move w-end-read            to fl-read
           end-if
           .
      *******************************************************************
      * Parametri di restart all'indietro                               *
      *******************************************************************
       rp-start-less.
           perform st-cogtrans-min
           if not w-verbo-ok
              move w-end-read            to fl-read
           end-if
           .
      *
       rp-start-equal.
           perform st-cogtrans-equal
           .
      *
       rp-fill-key.
           move w-t-key                  to trs-chia1
           .
      *
       rp-fill-key-from-b.
           move w-b-key                  to trs-chia1
           .
      *******************************************************************
      * Parametri di ricerca record                                     *
      *******************************************************************
       sn-read.
           perform rdnxt-cogtrans
           .
      *
       sp-read.
           perform rdprv-cogtrans
           .
      *
       rd-fill-t-key.
           move trs-chia1                to w-t-key
           .
      *
       rd-fill-b-key.
           move trs-chia1                to w-b-key
           .
      *******************************************************************
      * Controllo se smettere di leggere                                *
      *******************************************************************
       rd-controlli-read.
           .
      *******************************************************************
      * Controllo se utilizzare il record letto                         *
      *******************************************************************
       rd-controlli-fill.
           move "S"                      to fl-fill

           if az-personal = k-personal-unione-ag-bo and
                        trs-ana-tipo = 'C'          and
                        w-caller = "COSO16"
              move trs-ana-chia to anas-chia
              perform rd-unanaso
              if w-verbo-ok
                 move anas-ggdimis          to w-anas-ggdimis
                 move anas-mmdimis          to w-anas-mmdimis
                 move anas-aadimis          to w-anas-aadimis
                 if anas-dadimisn = 0 or
                    w-anas-dadimis  > datasis
                    continue
                 else
                    move 'N'      to fl-fill
                 end-if
              end-if
           end-if.
      *
           if w-filtro-tipologia (1) not = spaces or
              w-filtro-tipologia (2) not = spaces
              initialize       app-rec
              move trs-ana-chia to app-chia
              perform rd-cogappog
              if w-verbo-invalido
                 move "N" to fl-fill
              end-if
              if w-verbo-ok
                 if app-tip-stt  = w-filtro-tipologia (1) or
                    app-tip-stt  = w-filtro-tipologia (2)
                      continue
                   else
                      move 'N'      to fl-fill
                 end-if
              end-if
           end-if
      *
     **
           if tm-no-canc = "S" and trs-val not = spaces
                      move 'N'      to fl-fill
           end-if
     **
           if tm-fl-cli-for = "N" and ( trs-selezione = "C" or "F")
                      move 'N'      to fl-fill
           end-if
     **
           if tm-fl-contatti = "N" and ( trs-selezione = "O")
                      move 'N'      to fl-fill
           end-if
     **
           if tm-fl-destinazioni = "N" and ( trs-selezione = "D")
                      move 'N'      to fl-fill
           end-if
      *
           if fl-fill = "S"
              add 1                      to n-m
              move n-m                   to nm
              move spaces                to fl-read
              perform rd1-fill-tab
           end-if
           .
       rd1-fill-tab.
           move spaces                   to fl-read
           move n-m                      to nm
           perform rd1-ft-go
           .
      *
       rd1-ft-go.
           move trs-tip                  to tm-r-tip (nm)
           move trs-cod                  to tm-r-cod (nm)
           move trs-cod-padre            to tm-r-cod-padre (nm)
           move trs-val                  to tm-r-val (nm)
           move trs-desc                 to tm-r-desc (nm)
           move trs-desc2                to tm-r-desc2 (nm)
           move trs-indi                 to tm-r-indi (nm)
           move trs-cap                  to tm-r-cap (nm)
           move trs-dst                  to tm-r-dst (nm)

190117**--------------------------------------------------------------**
           move spaces                   to tm-r-desc-dst (nm)
           move spaces                   to tm-r-indi-dst (nm)
           move spaces                   to tm-r-cap-dst  (nm)
           move spaces                   to tm-r-loca-dst (nm)
           move spaces                   to tm-r-prov-dst (nm)
           if trs-dst-id-tip not = spaces
              move trs-r-desc-dst        to tm-r-desc-dst (nm)
              move trs-r-indi-dst        to tm-r-indi-dst (nm)
              move trs-r-cap-dst         to tm-r-cap-dst  (nm)
              move trs-r-loca-dst        to tm-r-loca-dst (nm)
              move trs-r-prov-dst        to tm-r-prov-dst (nm)
           else
              initialize dst-rec
              move trs-cod                 to dst-ana-cod
              move trs-tip                 to dst-ana-tip
              move "D"                     to dst-id-tip
              move spaces                  to w-fl-ok-dst
              perform st-cogdesti-notmin
              if w-verbo-ok
                perform rdnxt-cogdesti
                perform until w-fine-file  or
                            dst-ana-cod not = trs-cod  or
                            dst-ana-tip not = trs-tip  or
                            dst-id-tip  not = "D"      or
                            w-fl-ok-dst = "S"

                  if dst-fl-canc = spaces
                      if dst-fl-default = 'S'
                       move "S"              to w-fl-ok-dst

                       move dst-rag-soc      to tm-r-desc-dst (nm)
                       move dst-ind          to tm-r-indi-dst (nm)
                       if dst-cap-alfa not = spaces
                          move dst-cap-alfa  to tm-r-cap-dst  (nm)
                       else
                          move dst-cap       to tm-r-cap-dst  (nm)
                       end-if
                       move dst-loca         to tm-r-loca-dst (nm)
                       move dst-prov         to tm-r-prov-dst (nm)
                    end-if
                  end-if

                  perform rdnxt-cogdesti
                end-perform
              end-if
           end-if

           move trs-selezione               to tm-r-selezione (nm)
           evaluate trs-selezione
                when "C" move m-cliente     to tm-r-selezione (nm)
                when "F" move m-fornitore   to tm-r-selezione (nm)
                when "O" move m-contatto    to tm-r-selezione (nm)
                when "D" move m-destinazione   to tm-r-selezione (nm)
           end-evaluate

190117**--------------------------------------------------------------**

           initialize  tm-r-cod-iban (nm)
           move trs-app-iban-st          to tm-app-iban-st (nm)
           move trs-app-iban-cin-e       to tm-app-iban-cin-e (nm)
           move trs-app-iban-cin-it      to tm-app-iban-cin-it (nm)
           move trs-app-abi              to tm-app-abi (nm)
           move trs-app-cab              to tm-app-cab (nm)
           move trs-app-c-corr           to tm-app-c-corr (nm)

           move trs-r-cod-rif            to tm-r-cod-rif (nm)

           move trs-prov                 to tm-r-prov (nm)
           move trs-loca                 to tm-r-loca (nm)
                                            tm-r-loca-40 (nm)
           move trs-tel                  to tm-r-tel (nm)
           move trs-tel                  to tm-r-tel-btn (nm)
           if trs-sms (1:1) numeric and
              trs-sms (1:1) > 0
              move trs-sms               to tm-r-sms (nm)
              move trs-sms               to tm-r-sms-btn (nm)
            else
              move " "                   to tm-r-sms (nm)
              move " "                   to tm-r-sms-btn (nm)
           end-if
           move trs-fax                  to tm-r-fax (nm)
           move trs-piv                  to tm-r-piv (nm)
           move trs-cfi                  to tm-r-cfi (nm)
           move trs-email                to tm-r-email (nm)
           move trs-web                  to tm-r-web (nm)
           move trs-tp-stt-des           to tm-tp-stt-des (nm)
           move trs-rif-key              to tm-r-rif-key (nm)
           move trs-rif-nome             to tm-r-rif-nome (nm)
           move trs-age-cod              to tm-r-age-cod (nm)
           if trs-age-cod = 0
              move " "                  to tm-r-age-des (nm)
            else
              initialize ana-rec
              move "F"                   to ana-tipo
              move trs-age-cod           to ana-cod
              perform rd-coggeana
              if w-verbo-invalido
                 move m-agente-cancellato    to ana-nome
              end-if
              move ana-nome             to tm-r-age-des (nm)
           end-if
           move trs-old-cod             to tm-r-old-cod (nm)
           .
      *******************************************************************
      *Attivo e disattivo i t.f. utilizzabili sulla griglia di dettaglio*
      *******************************************************************
       x-attiva-tf-grid.
           perform x-attiva-sf4-griglia
           perform x-attiva-sf5-esporta-excel
           perform x-attiva-invio-seleziona
           perform x-attiva-sf7-contatti
           perform x-attiva-f1-configura
           if w06-in-tipo = "C" or
              az-cli-pot  = "S"
              perform x-attiva-f6-scheda
           end-if
           perform x-attiva-f5-ricerche
           if w06-in-tipo = "C"
              move k-sf8-ins-provv           to w-pbc-idx
              perform x-attiva-pbc
           end-if
           .
      *
       x-disattiva-tf-grid.
           perform x-disattiva-sf4-griglia
           perform x-disattiva-sf5-esporta-excel
           perform x-disattiva-invio-seleziona
           perform x-disattiva-f6-scheda
           perform x-disattiva-sf7-contatti
           perform x-disattiva-f1-configura
           perform x-disattiva-f5-ricerche
           move k-sf8-ins-provv           to w-pbc-idx
           perform x-disattiva-pbc
           .
      *******************************************************************
      * Riempimento del record con cui aggiornare la grid               *
      *******************************************************************
       x-riempi-grid-record.
           initialize grid-record
           move tm-r-cod (k)             to grd-cod
           move tm-r-desc (k)            to grd-desc
           move tm-r-desc2 (k)           to grd-desc2
           if tm-r-val (k) = "C"
              move "C"                   to grd-val
           else
              move "  "                  to grd-val
           end-if
           move tm-r-indi (k)            to grd-indi
           move tm-r-cap (k)             to grd-cap
           move tm-r-loca-40 (k)         to grd-loca
           move tm-r-prov (k)            to grd-prov
           move tm-r-tel (k)             to grd-tel
           move tm-r-fax (k)             to grd-fax
           move tm-r-piv (k)             to grd-piv
           move tm-r-cfi (k)             to grd-cfi
           move tm-r-email (k)           to grd-email
           move tm-r-web (k)             to grd-web
           move tm-r-cod-padre (k)       to grd-cod-padre
           move tm-tp-stt-des (k)        to grd-tp-stt-des
           move tm-r-rif-nome (k)        to grd-rif-nome

190117**--------------------------------------------------------------**
           move tm-r-desc-dst  (k)       to grd-desc-dst
           move tm-r-indi-dst  (k)       to grd-indi-dst
           move tm-r-cap-dst   (k)       to grd-cap-dst
           move tm-r-loca-dst  (k)       to grd-loca-dst
           move tm-r-prov-dst  (k)       to grd-prov-dst
           move tm-r-selezione (k)       to grd-selezione
           move tm-r-dst-idx (k)         to grd-dst

190117**--------------------------------------------------------------**
           .
      *******************************************************************
      * Chiamata per aprire la posta elettronica                        *
      *******************************************************************
       s2-mail.
           move tm-r-tip (i)             to ext-ana-tip
           move tm-r-cod (i)             to ext-ana-cod

           initialize util-posta
           if tm-r-email (i) not = spaces
              move tm-r-email (i)         to posta-email
           end-if
           move "Solo preparazione"      to posta-percorso
           call "POSTA" using stringhe util-posta
           cancel "POSTA"
           .
       s2-99.
       ex-s2. exit.
      *******************************************************************
      * Chiamata per aprire il browser                                  *
      *******************************************************************
       s3-web.
           move tm-r-tip (i)             to ext-ana-tip
           move tm-r-cod (i)             to ext-ana-cod

           initialize util-brws
           move "Open-da-Ana"            to brws-ope
           call "BROWSER" using stringhe util-brws
           cancel "BROWSER"
           .
       s3-99.
       ex-s3. exit.
      *******************************************************************
      * carica-tab                                                      *
      *******************************************************************
       carica-tab.
           modify e-tm-tab, mass-update = 1
           move 1                        to i
           perform z-svuota-tutto-grid
           modify e-tm-tab, mass-update = 0
           initialize tm-tab
           move zero                     to n-m
           compute w-t-size = w-grid-lines - 1
           move w-t-size                 to t-length t-size

           perform x-set-m-win058-9-1-id
           move m-win058-9-1 to wb-msg
           perform clock-msg
           perform x-spaginamento-start
           perform rem-clk
           move 1                        to i
           perform z-costruisci-grid
           if n-m not = zero
              modify e-tm-tab, cursor-x = 1,
                               cursor-y = 2
           else
              modify e-tm-tab, cursor-x = 1,
                               cursor-y = 1
           end-if
           move 1                        to i
           .
       ex-carica-tab. exit.

       x-spaginamento-start.
           initialize trs-rec
           move spaces                   to fl-read
           perform rst-not-less
           .
      *******************************************************************
      * Leggo la configurazione                                         *
      *******************************************************************
       x99-src-cfg.
           move 6                        to ncnf-pnt
           move k-pack-coge              to ncnf-pac-gen

           move "RICEGENERA"                      to ncnf-gruppo (01)
           move "RICERCA-UPPER"                   to ncnf-chiave (01)

           move "COLORI"                          to ncnf-gruppo (02)
           move "RIGA-GRIGLIA-WIN058-CONTATTO"    to ncnf-chiave (02)

           move "ABILITA"               to ncnf-gruppo (03)
           move "AREASMS"               to ncnf-chiave (03)

           move "RICEGENERA"             to ncnf-gruppo (04)
           move "escludi-anagrafiche-cancellate" to ncnf-chiave (04)

           move "azienda"                to ncnf-gruppo (05)
           move "ricerca-ana-su-destinazioni" to ncnf-chiave (05)

           move "COLORI"                          to ncnf-gruppo (06)
           move "riga-griglia-win058-dest"        to ncnf-chiave (06)

           call "RNEWCONF" using stringhe util-ncnf ncnf-tab
           cancel "RNEWCONF"
           move ncnf-valore (01)         to tm-cfg-upper
           move ncnf-valore-n (02)       to tm-cfg-colore-riga-rif
           move ncnf-valore (03)         to tm-cfg-abi-areasms
           move ncnf-valore (04)         to tm-cfg-no-canc
           move ncnf-valore (05)         to tm-cfg-destinazioni
           move ncnf-valore-n (06)       to tm-cfg-colore-riga-dst

           .
      *******************************************************************
      * x999-src-cfg                                                    *
      *******************************************************************
       x999-src-cfg.
           initialize euta-rec
           move 70                 to euta-tipo
           move 'RICE'             to euta-cfg-arg
           move 'DEFINE'           to euta-cfg-key
           perform rd-cogtabel
           if (w-verbo-invalido)
              perform x-set-m-win058-10-1-id
              move m-win058-10-1 to wb-1
              perform box-msg
              move k-no            to tm-cfg-ricerca-strif
           end-if

           if (euta-c008-rif <> 'N')
              and (euta-c008-rif <> 'S')
              move k-no            to tm-cfg-ricerca-strif
           else
              move euta-c008-rif   to tm-cfg-ricerca-strif
           end-if
           .
      *******************************************************************
      * Trasformo la stringa di input in uppercase                      *
      *******************************************************************
       a-uppercase.
           call "C$TOUPPER" using w06-in-str, value 30.
           .
      *******************************************************************
      * Cerco le configurazioni                                         *
      *******************************************************************
       d-load-cfg.
           move zero                     to n-l
           perform d1-src-len euta-c010-n-len times.
           move zero                     to n-i i
           move 1                        to n-d
           move " "                to tm-d (01)
           perform d2-src-del euta-c010-n-del times.
           .
      *
       d1-src-len.
           add 1                         to n-l
           move euta-c010-len (n-l)      to tm-l (n-l)
           if w-min = 0
              move tm-l (n-l)            to w-min
           else
              if tm-l (n-l) < w-min
                 move tm-l (n-l)         to w-min
              end-if
           end-if
           .
      *
       d2-src-del.
           add 1                         to i
           if euta-c010-d-tip (i) = "I"
              add 1                      to n-i
              move euta-c010-d-pic (i)   to tm-i (n-i)
           else
              add 1                      to n-d
              move euta-c010-d-pic (i)   to tm-d (n-d)
           end-if
           .
      *******************************************************************
      * Calcolo lunghezza stringa                                       *
      *******************************************************************
       src-len.
           move w06-in-str               to w-stringa-inp
           move 40                       to i-si
           perform src-len-loop until i-si < 1 or
                                      wsi-pic (i-si) not = spaces
                                          .
           .
       src-len-loop.
           subtract 1                  from i-si
           .
      *******************************************************************
      * Esplodo la stringa in varie sottostringhe escludendo i          *
      * delimitatori da ignorare                                        *
      *******************************************************************
       b-esplodi.
           set i-si                      to 1
           set i-so                      to 1
           move i-so                     to n-so
           set j-so                      to 1
           perform b-loop w-len times.
           if j-so not = 1
              compute tso-len (i-so) = j-so - 1
           end-if
           .
      *******************************************************************
      * Leggo un carattere della stringa di input; stabilisco se        *
      * considerarlo, ignorarlo o utilizzarlo per saltare la parola     *
      *******************************************************************
       b-loop.
           set i-d                       to 1
           perform b-src-d until i-d > n-d or
                                 wsi-pic (i-si) = tm-d (i-d).
           set i-i                       to 1
           perform b-src-i until i-i > n-i or
                                 wsi-pic (i-si) = tm-i (i-i).
           .
      *******************************************************************
      * Caso particolare per ',' e '.'; se uno di questi campi si trova *
      * tra due caratteri numerici non lo considero come delimitatore   *
      *******************************************************************
           if wsi-pic (i-si) = "." or
              wsi-pic (i-si) = ","
              if i-si not = 1 and
                 i-si not = w-len
                 if wsi-pic (i-si - 1) numeric and
                    wsi-pic (i-si + 1) numeric
                    compute i-d = n-d + 1
                    compute i-i = n-i + 1
                 end-if
              end-if
           end-if
      *******************************************************************
      * Il carattere ? nei separatori-parola: mi posiziono sulla stringa*
      * di output successiva e lo ignoro;                               *
      * Il carattere ? nei delimitatori da ignorare: lo ignoro          *
      *******************************************************************
           if i-d not > n-d
              compute tso-len (i-so) = j-so - 1
              set i-so                   up by 1
              move i-so                  to n-so
              set j-so                   to 1
           else
              if i-i > n-i
                 move wsi-pic (i-si)     to tso-pic (i-so j-so)
                 set j-so                up by 1
              end-if
           end-if
           set i-si                      up by 1
           .
      *
       b-src-d.
           set i-d                       up by 1
           .
      *
       b-src-i.
           set i-i                       up by 1
           .
      *******************************************************************
      * Da ogni sottostringa creata, creo tutte le stringhe di output   *
      *******************************************************************
       c-stringhe.
           move zero                     to w58-t
           set i-so                      to 1
           perform c-str-loop until i-so > n-so or
                                    w58-t = 3.
           .
      *******************************************************************
      * Creo le stringhe di output per una lunghezza                    *
      *******************************************************************
       c-str-loop.
           if tso-len (i-so) not < w-min
              perform c-length-ok
           end-if
           set i-so                      up by 1
           .
      *******************************************************************
      * In base al tipo di lenghezza considerata:                       *
      * - Inizio parola       : creo la stringa una sola volta          *
      *******************************************************************
       c-length-ok.
           perform check-len
           perform c-fill-word
           .
      *
       c-fill-word.
           move spaces                   to w-parola
           set i-wp                      to 1
           perform c-fill-pic w-str times.
           add 1                         to w58-t
           move w-parola                 to w58-paro (w58-t)
           .
      *
       c-fill-pic.
           move tso-pic (i-so i-wp)      to wp-pic (i-wp)
           set i-wp                      up by 1
           .
       check-len.
           move zero                     to w-str
           move zero                     to n-l
           perform c1-ck-len euta-c010-n-len times.
           if w-str = zero
              move zero                  to n-l
              perform c2-ck-len euta-c010-n-len times.
           .
      *
       c1-ck-len.
           add 1                         to n-l
           if tm-l (n-l) = tso-len (i-so)
              move tm-l (n-l)            to w-str
           end-if
           .
      *
       c2-ck-len.
           add 1                         to n-l
           if w-str = zero
              if tm-l (n-l) < tso-len (i-so)
                 move tm-l (n-l)         to w-str
              end-if
           else
              if tm-l (n-l) > w-str and
                 tm-l (n-l) < tso-len (i-so)
                 move tm-l (n-l)         to w-str
              end-if
           end-if
           .
      *******************************************************************
      * Ricerca anagrafiche x le stringhe accettate                     *
      *******************************************************************
       creo-file.
           if w58-t = 0
              go to ex-creo-file
           end-if
           move 0                        to ctrp
           move 0                        to flagerr
           move 0                        to w-old
           set w58-i                     to 1
           perform creo-ric            thru ex-creo-ric
                                   until w58-i > w58-t.
           .
       cre-90.
           if flagerr = 0
              set w58-i                  to 1
              perform acc-par
              if w-old = w-min
                 go to ex-creo-file
              else
                 move w-par              to w-old
                 perform a-fill-word
                 set w58-i               to 1
                 perform creo-ric      thru ex-creo-ric
                 go to cre-90
              end-if
           end-if
           .
       cre-100.
           move 1                        to ind
           move zero                     to prog
           move 1                        to punt
           .
       cre-110.
           perform view-clk
           move all spaces               to stan-rec
           move old-chia                 to stan-paro1
           move prog                     to stan-prog1
           set stan-ind                  to ind
      *    .
      *cre-120.
           perform rd-cogstana
           if not w-verbo-ok
              go to ex-creo-file
           end-if
           if stan-tabe(stan-ind) not = zero
              go to cre-125
           end-if
           if punt = old-ulti
              go to ex-creo-file
           end-if
           add 1                         to punt
           if ind = 40
              move 1                     to ind
              add 1                      to prog
           else
              add 1                      to ind
           end-if
           go to cre-110
           .
       cre-125.
           move stan-tabe(stan-ind)      to old-tabe
           .
       cre-130.
           move zero                     to prog1
           move zero                     to ctrt
           set w58-i                     to 1
           perform creo-str            thru ex-creo-str
                                      until w58-i > w58-t.
           .
       cre-210.
           if ctrt > ctrp
              perform x-set-m-win058-11-1-id
              move m-win058-11-1 to wb-1
              perform box-msg
              go to ex-creo-file
           end-if
           if ctrt = ctrp
              perform creo-rec         thru ex-creo-rec
           end-if
           if punt = old-ulti
              go to ex-creo-file
           end-if
           add 1                         to punt
           if ind = 40
              move 1                     to ind
              add 1                      to prog
           else
              add 1                      to ind
           end-if
           go to cre-110
           .
       ex-creo-file. exit.
      *
       creo-ric.
           perform view-clk
           move spaces                   to stan-rec
           move w58-paro (w58-i)         to stan-paro1
           move zero                     to stan-prog1
           perform rd-cogstana
           if not w-verbo-ok
              go to ric-20
           end-if
           add 1                         to ctrp
           move 1                        to flagerr
           if old-ulti = 0
              move stan-ulti             to old-ulti
              move w58-paro (w58-i)      to old-chia
           else
              if stan-ulti < old-ulti
                 move stan-ulti          to old-ulti
                 move w58-paro (w58-i)   to old-chia
              end-if
           end-if
           .
       ric-20.
           set w58-i                     up by 1
           .
       ex-creo-ric. exit.
      *
       creo-str.
           perform view-clk
           if w58-paro (w58-i) = old-chia
              add 1                      to ctrt
              go to str-50
           end-if
           if w58-paro (w58-i) = spaces
              go to str-50
           end-if
           .
       str-10.
           move spaces                   to stan-rec
           move w58-paro (w58-i)         to stan-paro1
           move prog1                    to stan-prog1
           perform rd-cogstana
           if not w-verbo-ok
              go to str-50
           end-if
           .
       str-30.
           set stan-ind                  to 1
           .
       str-40.
           search stan-tabe
                  when stan-tabe(stan-ind) = old-tabe
                  add 1                  to ctrt.
           add 1                         to prog1
           go to str-10
           .
       str-50.
           move zero                     to prog1
           set w58-i                     up by 1
           .
       ex-creo-str. exit.
      *
       creo-rec.
           perform view-clk
           move old-tabe                 to ran-nume
           perform rd-cogracan
           if not w-verbo-ok
              move all "*"               to trs-rec
           end-if
           if w06-in-tipo not = ran-tipo
              go to ex-creo-rec
           end-if
           initialize ana-rec
           move ran-tipo                 to ana-tipo app-tipo trs-tip
           move ran-ana                  to trs-cod ana-cod app-cod
           perform rd-coggeana
           if not w-verbo-ok
              move m-cancellato          to ana-nome
           end-if
           initialize trs-rif-key trs-rif-nome
           perform fill-trs-sub-coggeana
           move ana-tipo to trs-selezione
           perform wr-cogtrans
           .
       ex-creo-rec. exit.
      *
       fill-trs-sub-coggeana.
           move ana-cod                  to trs-cod
           move ana-val                  to trs-val
           move ana-nome                 to trs-desc
           move ana-nome2                to trs-desc2
           move ana-ind                  to trs-indi
           move ana-loca                 to trs-loca
           move ana-prov                 to trs-prov
           move ana-cap                  to trs-cap
           move ana-chia                 to trs-ana-chia

           initialize aap-rec.
           move ana-tipo     to aap-ana-tip.
           move ana-cod      to aap-ana-cod.
           perform rd-coganapp.
           if w-verbo-ok
170314        if aap-cap not = spaces
                 move aap-cap  to trs-cap
170314        end-if
170314        if aap-loca not = spaces
                 move aap-loca to trs-loca
170314        end-if
              move aap-old-cod           to trs-old-cod
           end-if.

           move ana-agente               to trs-age-cod
           move ana-padre                to trs-cod-padre

           initialize app-rec.
           move ana-tipo     to app-tipo
           move ana-cod      to app-cod

           perform rd-cogappog
           if w-verbo-invalido
              initialize app-rec
           end-if
           move app-tel                  to trs-tel
           move app-fax                  to trs-fax
           move ana-piva                 to trs-piv
           move ana-cofi                 to trs-cfi
           move ana-email                to trs-email
           move ana-cod-rif              to trs-r-cod-rif
      *-----------------------------------------------------------------
      * Ricerco un numero valido per la spedizione SMS
      *-----------------------------------------------------------------
           move spaces                   to trs-sms
           if app-tel (1:1) numeric and
              app-tel (1:1) > 0
              move app-tel               to trs-sms
           end-if

           move app-iban-st              to trs-app-iban-st
           move app-iban-cin-e           to trs-app-iban-cin-e
           move app-iban-cin-it          to trs-app-iban-cin-it
           move app-abi                  to trs-app-abi
           move app-cab                  to trs-app-cab
           move app-c-corr               to trs-app-c-corr

           initialize trs-tp-stt-des
           if app-tipo = k-ana-tipo-cli
                         and
              app-tip-stt not = spaces
              initialize euta-rec
              move 84                    to euta-tipo
              move spaces                to euta-cod
              string app-tipo       delimited by size
                        app-tip-stt delimited by size
                     into euta-cod
              perform rd-cogtabel
              if  w-verbo-ok
                 move euta-descr         to trs-tp-stt-des
              end-if
           end-if
           move ana-tipo                 to cnd-tip
           move ana-cod                  to cnd-cod
           move "W1"                     to cnd-cnd
           move "1"                      to cnd-key
           perform rd-cogcndcf
           if not w-verbo-ok
              initialize cnd-dati
           end-if
           move cnd-dati                 to trs-web
           .
      *
       acc-par.
           perform view-clk
           move w58-paro (w58-i)         to w-parola
           move 9                        to i-wp
           perform acc-len-loop until wp-pic (i-wp) not = spaces or
                                      i-wp < 1.
           move i-wp                     to w-par
           perform acc-check-len
           .
       acc-len-loop.
           subtract 1                  from i-wp
           .
       acc-check-len.
           perform view-clk
           move zero                     to w-str
           move zero                     to n-l
           perform a2-ck-len euta-c010-n-len times.
           if w-str not = zero
              move w-str                 to w-par
           end-if
           .
      *
       a2-ck-len.
           add 1                         to n-l
           if w-str = zero
              if tm-l (n-l) < w-par
                 move tm-l (n-l)         to w-str
              end-if
           else
              if tm-l (n-l) > w-str and
                 tm-l (n-l) < w-par
                 move tm-l (n-l)         to w-str
              end-if
           end-if
           .
      *
       a-fill-word.
           move spaces                   to w-parola1
           set i-wp1                     to 1
           set i-wp                      to 1
           perform a-fill-pic w-par times.
           move 1                        to w58-t
           move w-parola1                to w58-paro (w58-t)
           .
      *
       a-fill-pic.
           perform view-clk
           move wp-pic (i-wp)            to wp1-pic (i-wp1)
           set i-wp1                     up by 1
           set i-wp                      up by 1
           .
       search-file-s.
            go to sf-01-s
                                  depending on w-ind-fl.
       sf-01-s.
            open input  unanaso.
            go to ex-search-file-s.
       ex-search-file-s.
            exit.
       z-ricerca-cogdesti.
      *------------------------------------------------------------------
      * Se e' stata eseguita una ricerca sull'oggetto, creo ora le
      * chiavi di ricerca
      *------------------------------------------------------------------
           if w06-in-str <> " "
              initialize util-gesstr
              move "CERCA-PREPARA"         to gesstr-ope
              move w06-in-str              to gesstr-note
              move function length(w06-in-str) to gesstr-len-note
              call "GESSTR"             using stringhe util-gesstr
                                              gesstr-note
                                              gesstr-tab-righe
                                              gesstr-tab-righe-agg
           else
             go to ex-ricerca-cogdesti
           end-if
           move spaces                   to w-fl-esito-ragsoc
           move spaces                   to w-fl-esito-indi
           move spaces                   to w-fl-esito-loca

            move low-value    to dst-rec
            move w06-in-tipo  to dst-ana-tip
            perform st-cogdesti-notmin
            if w-verbo-ok
               perform rdnxt-cogdesti
               perform until w-fine-file or
                             dst-ana-tip not = w06-in-tipo
                       if dst-id-tip = "D"
                         perform ricerca-su-desti
                         if w-fl-esito-ragsoc = "S" or
                          w-fl-esito-indi   = "S" or
                          w-fl-esito-loca   = "S"
                          initialize trs-rec ana-rec
                          move dst-ana        to ana-chia
                          perform rd-coggeana
                          if w-verbo-invalido
                             move all "*" to ana-rec
                          end-if
                          move dst-ana to trs-ana-chia
                          move dst-id  to trs-dst
                          perform fill-trs-sub-coggeana

                          move dst-rag-soc      to trs-r-desc-dst
                          move dst-ind          to trs-r-indi-dst
                          if dst-cap-alfa not = spaces
                             move dst-cap-alfa  to trs-r-cap-dst
                          else
                             move dst-cap       to trs-r-cap-dst
                          end-if
                          move dst-loca         to trs-r-loca-dst
                          move dst-prov         to trs-r-prov-dst
                          move "D"              to trs-selezione
                          if dst-fl-canc not = spaces or
                             ana-val     not = spaces
                             move "S"           to trs-val
                          end-if
                          perform wr-cogtrans
                          if w-verbo-invalido
                              perform rwr-cogtrans
                          end-if
                       end-if
                     end-if
                     perform rdnxt-cogdesti
               end-perform
              end-if.
       ex-ricerca-cogdesti. exit.
      **
       ricerca-su-desti.
030816**--------------------------------------------------------------**
030816** Filtro la ricerca x la descrizione impostata
030816**--------------------------------------------------------------**

              move spaces                  to w-fl-esito-ragsoc
              move spaces                  to w-fl-esito-indi
              move spaces                  to w-fl-esito-loca

              initialize util-gesstr
              move "CERCA-ESEGUI"          to gesstr-ope
              move dst-rag-soc             to gesstr-note
              move function length(dst-rag-soc) to gesstr-len-note
              call "GESSTR"             using stringhe util-gesstr
                                              gesstr-note
                                              gesstr-tab-righe
                                              gesstr-tab-righe-agg

              move gesstr-esito            to w-fl-esito-ragsoc

              initialize util-gesstr
              move "CERCA-ESEGUI"           to gesstr-ope
              move dst-ind                  to gesstr-note
              move function length(dst-ind) to gesstr-len-note
              call "GESSTR"             using stringhe util-gesstr
                                              gesstr-note
                                              gesstr-tab-righe
                                              gesstr-tab-righe-agg

              move gesstr-esito            to w-fl-esito-indi

              initialize util-gesstr
              move "CERCA-ESEGUI"            to gesstr-ope
              move dst-loca                  to gesstr-note
              move function length(dst-loca) to gesstr-len-note
              call "GESSTR"             using stringhe util-gesstr
                                              gesstr-note
                                              gesstr-tab-righe
                                              gesstr-tab-righe-agg

              move gesstr-esito            to w-fl-esito-loca
           .
030816**--------------------------------------------------------------**

      *
       copy "grave.cpy".
       copy "mmmask.cpy".
       copy "opengen.cpy".
       copy "opens.cpy".
       copy "stato.cpy".
       copy "stato1.cpy".
       copy "winmsg.cpy".
      *
       copy "win058.prc".
       copy "dtab.cpy".
       copy "fscrol2.cpy".
       copy "utilpggr.cpy".
       copy "unanaso.k01".
      *
       copy "cogtrans.k01".
       copy "tran-cogsa1.k01".
      *
       end program.
