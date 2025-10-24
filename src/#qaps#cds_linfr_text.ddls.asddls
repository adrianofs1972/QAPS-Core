@AbapCatalog.sqlViewName: '/QAPS/V_LIFNR_TX'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Texto do Fornecedor'
@ObjectModel.dataCategory: #TEXT
@ObjectModel.representativeKey: 'Werks'
define view /QAPS/CDS_LIFNR_TEXT as select from lfa1 {
    key lfa1.lifnr,
        lfa1.name1 as lifnr_text
}
