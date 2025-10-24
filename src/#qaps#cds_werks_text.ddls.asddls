@AbapCatalog.sqlViewName: '/QAPS/V_WERKS_TX'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Texto do Centro'
@ObjectModel.dataCategory: #TEXT
@ObjectModel.representativeKey: 'Werks'
define view /qaps/cds_werks_text as select from t001w {
    key t001w.werks,
        t001w.name1 as werks_text
}
