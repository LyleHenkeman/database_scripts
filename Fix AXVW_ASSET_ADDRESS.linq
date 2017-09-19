<Query Kind="SQL">
  <Connection>
    <ID>ee885e94-f75f-47bd-a8a3-102f30613e63</ID>
    <Server>.\LYLE</Server>
    <Database>AXIOMSEED</Database>
    <ShowServer>true</ShowServer>
  </Connection>
  <Output>DataGrids</Output>
</Query>

--Hello, the error was that an upgrade step either failed or was missed.
--To Fix:

Step 1 - Run:
BEGIN
       BEGIN
              execute immediate 'CREATE OR REPLACE TYPE ASSET_HDR_ADDRESS_TBLOBJ AS OBJECT (asset_hdr_address_type NUMBER(10,0), address_id NUMBER(10,0))';
       EXCEPTION
              WHEN OTHERS THEN
                     null;
       END;
       --
       BEGIN
              execute immediate 'CREATE OR REPLACE TYPE ASSET_HDR_ADDRESS_TBLTAB AS TABLE OF ASSET_HDR_ADDRESS_TBLOBJ';
       EXCEPTION
              WHEN OTHERS THEN
                     null;
       END;
END;

Step 2 – Run:

create or replace FUNCTION AXSP_GET_ASSET_HDR_ADDRESS_TBL
       (p_asset_hdr_id IN NUMBER,
       p_effective_from IN DATE)
       RETURN ASSET_HDR_ADDRESS_TBLTAB PIPELINED 
IS
       v_ret_val NUMBER;
       cursor c1(c_asset_hdr_address_type number) is
              select effective_dt, address_id, asset_hdr_address_id from asset_hdr_address
              where asset_hdr_id = p_asset_hdr_id
              and asset_hdr_address_type = c_asset_hdr_address_type
              and effective_dt <= p_effective_from
              order by effective_dt desc, asset_hdr_address_id desc;
--
       ASSET_HDR_ADDRESS_TBL ASSET_HDR_ADDRESS_TBLOBJ := ASSET_HDR_ADDRESS_TBLOBJ (null, null);
BEGIN
       v_ret_val := 0;
       for i in 45100..45102 loop
              for j in c1(i) loop
                     ASSET_HDR_ADDRESS_TBL.asset_hdr_address_type := i;
                     ASSET_HDR_ADDRESS_TBL.address_id := j.address_id;
                     PIPE ROW(ASSET_HDR_ADDRESS_TBL);
                     exit; -- return first row of cursor only
              end loop;
       end loop;
       RETURN ;
END AXSP_GET_ASSET_HDR_ADDRESS_TBL;

Step 3 - Run:
CREATE OR REPLACE FORCE VIEW AXVW_ASSET_ADDRESS
AS
  SELECT ah.vo_type,
    ah.address_id,
    ah.contract_id,
    ah.asset_id,
    ah.asset_hdr_id,
    ah.original_purchase_dt,
    ah.name,
    ah.full_address,
    ah.sub_location,
    ah.asset_description,
    asset_type.name asset_type_name,
    manufacturer.name manufacturer,
    ah.model,
    ah.reference,
    ah.serial_no,
    ah.stock_code,
    ah.asset_type_id,
    ah.asset_status,
    asset_class_vehicle.registration_no,
    address.linked_party_address_id,
    ah.asset_hdr_address_type,
    ah.stamp
  FROM
    (SELECT asset.*,
      axsp_get_concat_address(asset.address_id) AS full_address
    FROM
      (SELECT 'VOAsset' vo_type,
        axsp_get_asset_address_id(asset.asset_id, axsp_dateonly(axsp_get_datetime())) AS address_id,
        asset.contract_id,
        asset.asset_id,
        asset.asset_hdr_id,
        asset_hdr.original_purchase_dt,
        asset_hdr.name,
        asset_hdr.sub_location,
        asset_hdr.asset_description,
        asset_hdr.model,
        asset_hdr.reference,
        asset_hdr.manufacturer_id,
        asset_hdr.serial_no,
        asset_hdr.stock_code,
        asset_hdr.asset_type_id,
        asset_hdr.asset_status,
        0 AS asset_hdr_address_type,
        asset.stamp
      FROM asset
      INNER JOIN asset_hdr
      ON asset_hdr.asset_hdr_id = asset.asset_hdr_id
      WHERE asset.asset_id+0 > 0
      ) asset
    UNION ALL
    SELECT asset_hdr.*,
      axsp_get_concat_address(asset_hdr.address_id) AS full_address
    FROM
      (SELECT 'VOAssetHdr' vo_type,
        ca.address_id,
        CASE
          WHEN asset_hdr.current_contract_id > 0
          THEN asset_hdr.current_contract_id
          ELSE asset_hdr.original_contract_id
        END contract_id,
        0 asset_id,
        asset_hdr.asset_hdr_id,
        asset_hdr.original_purchase_dt,
        asset_hdr.name,
        asset_hdr.sub_location,
        asset_hdr.asset_description,
        asset_hdr.model,
        asset_hdr.reference,
        asset_hdr.manufacturer_id,
        asset_hdr.serial_no,
        asset_hdr.stock_code,
        asset_hdr.asset_type_id,
        asset_hdr.asset_status,
        ca.asset_hdr_address_type,
        asset_hdr.stamp
      FROM asset_hdr
      cross join table(AXSP_GET_ASSET_HDR_ADDRESS_TBL(asset_hdr.asset_hdr_id, axsp_dateonly(axsp_get_datetime()))) ca
      WHERE asset_hdr.asset_hdr_id+0 > 0
      ) asset_hdr
    ) ah
  INNER JOIN asset_type
  ON ah.asset_type_id = asset_type.asset_type_id
  INNER JOIN party manufacturer
  ON ah.manufacturer_id = manufacturer.party_id
  INNER JOIN address
  ON ah.address_id = address.address_id
  LEFT OUTER JOIN asset_class_vehicle
  ON ah.asset_hdr_id = asset_class_vehicle.asset_hdr_id
