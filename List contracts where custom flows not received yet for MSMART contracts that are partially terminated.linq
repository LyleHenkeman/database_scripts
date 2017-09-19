<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

-- List contracts where custom flows not received yet for MSMART contracts that are partially terminated

SELECT   PRC.CONTRACT_ID, PRC.DESTINATION_CSV, CF.EXT_REFERENCE

  FROM   PO_ROW_CF PRC,

         CONTRACT_CUSTOM_FLOW CF,

         CONTRACT CT,

         PRODUCT PR,

         ASSET_HDR AH,

         CONTRACT CT1

WHERE       PRC.PO_ROW_CF_ID = CF.PO_ROW_CF_ID

         AND PRC.CONTRACT_ID = CF.CONTRACT_ID

         AND EXT_REFERENCE LIKE 'PO_ROW_%'

         AND CF.CONTRACT_ID = CT.CONTRACT_ID

         AND CT.PRODUCT_ID = PR.PRODUCT_ID

         AND PR.PRODUCT_ID = 47 -- MSMART PO

         AND PRC.NUM_ASSETS_RECEIVED = 0 -- Custom Flows Not Yet Received

         AND PRC.DESTINATION_CSV = AH.ASSET_HDR_ID

         AND AH.CURRENT_CONTRACT_ID = CT1.CONTRACT_ID

         AND CT1.CONTRACT_STATE = 11140 -- Partially Terminated Contract