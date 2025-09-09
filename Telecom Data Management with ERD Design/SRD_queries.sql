-- EXERCICE 1
SELECT FIRST_NAME, LAST_NAME, START_DATE, SERVICE_NAME
FROM CLIENT
INNER JOIN CONTRACT
ON CONTRACT.ID_CLIENT = CLIENT.ID_CLIENT
INNER JOIN CONTRACTED_SERVICE
ON CONTRACTED_SERVICE.ID_CONTRACT = CONTRACT.ID_CONTRACT
INNER JOIN SERVICE
ON CONTRACTED_SERVICE.ID_SERVICE = SERVICE.ID_SERVICE
WHERE (START_DATE BETWEEN STR_TO_DATE('2019-01-01', '%Y-%m-%d') AND STR_TO_DATE('2019-02-01', '%Y-%m-%d'));

-- 3 simple selects, from the desk, contract and rating tables. The desk type is All, the Contract is ref and the Rating is eq_ref. 
-- The keys are ID_DESK for CONTRACT, PRIMARY for the Ratings, DESK has a null key. The first step uses temporary, the second step uses Index. The last one has no extra info.

-- EXERCICE 2
SELECT * FROM SERVICE
ORDER BY MONTHLY_REFERENCE_COST DESC
LIMIT 3;

-- Simple select from the Services table, with 7 rows and using filesort.

-- EXERCICE 3
SELECT "2019/01/01 - 2021/01/01"as "Period of Sales", SUM(INVOICE.TOTAL_VALUE) AS 'Total Sales', SUM(INVOICE.TOTAL_VALUE)/2 AS 'Yearly Average', SUM(INVOICE.TOTAL_VALUE)/24 AS 'Monthly Average'
FROM INVOICE
WHERE (INVOICE.EMISSION_DATE BETWEEN STR_TO_DATE('2019-01-01', '%Y-%m-%d') AND STR_TO_DATE('2021-01-01', '%Y-%m-%d'));

-- Simple select from the invoices table, with 113 rows, of type all and using where.

-- EXERCICE 4
SELECT desk.location, COUNT(1) as 'contracts'
FROM CONTRACT
INNER JOIN DESK
ON DESK.ID_DESK = CONTRACT.ID_DESK
GROUP BY DESK.LOCATION;

-- 2 Simple selects from desk and contract tables. From DESK table it's of type all and uses temporary, yielding 3 rows. From the Contract table, 
-- it's by reference, using index, and the ID_DESK as Key, yielding 1 row.

-- EXERCICE 5
SELECT desk.location, COUNT(1) as 'contracts', AVG(RATING.RATING) AS 'AVERAGE RATING'
FROM CONTRACT
INNER JOIN DESK
ON DESK.ID_DESK = CONTRACT.ID_DESK
INNER JOIN RATING
ON RATING.ID_CONTRACT = CONTRACT.ID_CONTRACT
GROUP BY DESK.LOCATION;

-- 3 simple selects, from desk, contract and rating. The desk select uses temporary and the contract select uses index. The key used in the contract select is ID_DESK
-- and the key ysed in RATING is the primary key. There is no key in the DESK select. 

-- Step G
CREATE OR REPLACE VIEW invoice_head AS
	SELECT ID_INVOICE as 'Invoice ID', CLIENT.FIRST_NAME as 'First Name', CLIENT.LAST_NAME as 'Last Name', CLIENT.STREET as 'Street',
		CLIENT.CITY as 'City', CLIENT.TIN as 'TIN', CLIENT.PHONE as 'Client Contact', EMISSION_DATE as 'Emission Date', 
        PAYMENT_DEADLINE as 'Deadline', TOTAL_VALUE as 'Total (€)'
	FROM INVOICE
    INNER JOIN CONTRACT ON INVOICE.ID_CONTRACT = CONTRACT.ID_CONTRACT
    INNER JOIN CLIENT ON CONTRACT.ID_CLIENT = CLIENT.ID_CLIENT;

SELECT * FROM invoice_head;

CREATE OR REPLACE VIEW invoice_extra AS
	SELECT INVOICE.ID_INVOICE as 'Invoice ID', SERVICE.SERVICE_NAME as 'Contracted Service Name', 
		SERVICE.SERVICE_DESCRIPTION as 'Contracted Service Description', SERVICE.MONTHLY_REFERENCE_COST as 'Monthly Reference Cost (€)', 
        SERVICE.MONTHLY_REFERENCE_COST-INVOICE.TOTAL_VALUE as 'Discount (€)', INVOICE.TOTAL_VALUE as 'Actual Cost (€)'
	FROM INVOICE
    INNER JOIN CONTRACT ON INVOICE.ID_CONTRACT = CONTRACT.ID_CONTRACT
    INNER JOIN INVOICE_DETAIL ON INVOICE.ID_CONTRACT = INVOICE_DETAIL.ID_CONTRACT
    INNER JOIN SERVICE ON INVOICE_DETAIL.ID_SERVICE = SERVICE.ID_SERVICE;
    
SELECT * FROM invoice_extra;