@AbapCatalog.sqlViewName: '/QAPS/V_KUNNR_TX'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Texto do Cliente'
@ObjectModel.dataCategory: #TEXT
@ObjectModel.representativeKey: 'Werks'
define view /QAPS/CDS_KUNNR_TEXT as select from kna1 {
    key kna1.kunnr,
        kna1.name1 as kunnr_text
}
