@AbapCatalog.sqlViewName: '/QAPS/V_TP_OD_TX'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Origem/Destino Text'
define view /QAPS/CDS_TIPO_ORIG_DEST_TEXT as 
select from dd07v {    
    domvalue_l as valor,    
    ddtext as texto
}
where domname = '/QAPS/D_TIPO_PONTO'
