import time

from django.http import JsonResponse, HttpResponseNotFound
from django.shortcuts import render

# Create your views here.

json1 = {
    "message": "操作成功",
    "data": {
        "total": 20,
        "rows": [{
            "contractType": "businessQuota",
            "businessName": "额度法人账户透支",
            "manageStaff": "非洲支行客户经理abc",
            "updatedDate": 1626364800000,
            "customerId": "20210531100223000013",
            "occurType": "OT01",
            "registrationDate": 1626364800000,
            "currency": "rmb",
            "id": "89930391552532480",
            "quotaIs": "1",
            "delflag": "N",
            "riskClassifyPriRes": "PRORC01",
            "registry": "营业部",
            "registerPer": "李国栋",
            "rev": 0,
            "contractStatus": "02",
            "agency": "营业部",
            "manager": "李国栋",
            "approveNo": "appr20210713163919000013",
            "staffOrg": "非洲区支行",
            "occurDate": 1626105600000,
            "customerName": "非洲客户abc",
            "quotaBalanceAmount": 0,
            "proProperty": "quota",
            "applyNo": "appl20210713162038000008",
            "businessIs": "1",
            "wfStatus": "10",
            "businessType": "pord1982389",
            "contractSerialNo": "cont20210716145605000001"
        }, {
            "contractType": "businessQuota",
            "manageStaff": "李国栋",
            "updatedDate": 1626105600000,
            "customerId": "20210531144319000020",
            "occurType": "OT01",
            "registrationDate": 1626105600000,
            "currency": "rmb",
            "id": "88762334499184640",
            "quotaIs": "1",
            "delflag": "N",
            "riskClassifyPriRes": "PRORC01",
            "registry": "营业部",
            "registerPer": "李国栋",
            "rev": 0,
            "contractStatus": "02",
            "agency": "营业部",
            "manager": "李国栋",
            "approveNo": "appr20210603175926000007",
            "staffOrg": "营业部",
            "occurDate": 1622736000000,
            "customerName": "中电",
            "quotaBalanceAmount": 0,
            "applyNo": "appl20210603103408000003",
            "businessIs": "1",
            "wfStatus": "10",
            "businessType": "prod077",
            "contractSerialNo": "cont20210713093439000001"
        }, {
            "quotaNominAmount": 1111.000000,
            "contractType": "businessQuota",
            "rateAdjustMeth": "RAM05",
            "repayCycle": "Q",
            "baseRateType": "01",
            "updatedDate": 1622736000000,
            "interestRateModel": "IRM02",
            "mainGuarantyMeth": "VT01",
            "occurType": "OT01",
            "registrationDate": 1622736000000,
            "id": "74710588923584512",
            "repayMeth": "04",
            "delflag": "N",
            "riskClassifyPriRes": "PRORC01",
            "paymentMeth": "01",
            "registerPer": "肖隆",
            "rev": 3,
            "rateFloatValue": 12.000000,
            "settleAccountName": "12312",
            "staffOrg": "风险管理部",
            "baseAnnualRate": 12.000000,
            "customerName": "测试一号",
            "quotaBalanceAmount": 0,
            "wfStatus": "10",
            "startDate": 1622736000000,
            "contractSerialNo": "cont20210604145802000027",
            "executeAnnualRate": 12.000000,
            "manageStaff": "钟孙剑",
            "expiryDate": 1628006400000,
            "rateFloatMeth": "RFM01",
            "customerId": "20210519162322000021",
            "currency": "rmb",
            "term": 2,
            "quotaIs": "1",
            "registry": "风险管理部",
            "contractStatus": "02",
            "agency": "风险管理部",
            "manager": "肖隆",
            "marginRatio": 14.000000,
            "approveNo": "appr20210521200518000051",
            "rateAdjustCycle": "",
            "occurDate": 1622736000000,
            "industryInvest": "    小麦种植",
            "creditOrganiz": "CO01",
            "termApplyType": "TT02",
            "applyNo": "appl20210521190742000039",
            "businessIs": "1",
            "accountInstitute": "风险管理部",
            "businessType": "Y01",
            "settleAccount": "1231231"
        }, {
            "quotaNominAmount": 0.000000,
            "contractType": "businessQuota",
            "rateAdjustMeth": "RAM06",
            "repayCycle": "Y",
            "baseRateType": "01",
            "updatedDate": 1622476800000,
            "interestRateModel": "IRM02",
            "mainGuarantyMeth": "VT02",
            "occurType": "OT01",
            "registrationDate": 1622476800000,
            "id": "73566587323555840",
            "repayMeth": "03",
            "delflag": "N",
            "riskClassifyPriRes": "PRORC01",
            "paymentMeth": "01",
            "registerPer": "肖隆",
            "rev": 3,
            "rateFloatValue": 123.000000,
            "settleAccountName": "123123",
            "staffOrg": "风险管理部",
            "baseAnnualRate": 123.000000,
            "customerName": "测试二号",
            "quotaBalanceAmount": 0,
            "wfStatus": "10",
            "startDate": 1622476800000,
            "contractSerialNo": "cont20210601111211000001",
            "executeAnnualRate": 123.000000,
            "manageStaff": "钟孙剑",
            "expiryDate": 1622476800000,
            "rateFloatMeth": "RFM01",
            "customerId": "20210526141000000045",
            "currency": "rmb",
            "term": 12,
            "quotaIs": "1",
            "registry": "风险管理部",
            "contractStatus": "02",
            "agency": "风险管理部",
            "manager": "肖隆",
            "marginRatio": 0.000000,
            "approveNo": "appr20210526163337000083",
            "rateAdjustCycle": "01",
            "occurDate": 1622476800000,
            "industryInvest": "    稻谷种植",
            "creditOrganiz": "CO01",
            "termApplyType": "TT03",
            "applyNo": "appl20210526160338000067",
            "businessIs": "1",
            "accountInstitute": "信息科技部",
            "businessType": "Y01",
            "settleAccount": "1231"
        }, {
            "quotaNominAmount": 0.000000,
            "contractType": "businessQuota",
            "manageStaff": "钟孙剑",
            "updatedDate": 1622476800000,
            "customerId": "20210519162322000021",
            "occurType": "OT01",
            "registrationDate": 1622476800000,
            "currency": "rmb",
            "term": 12,
            "id": "73569003783725056",
            "quotaIs": "1",
            "delflag": "N",
            "riskClassifyPriRes": "PRORC01",
            "registry": "风险管理部",
            "registerPer": "肖隆",
            "rev": 0,
            "contractStatus": "02",
            "agency": "风险管理部",
            "manager": "肖隆",
            "approveNo": "appr20210521200518000051",
            "staffOrg": "风险管理部",
            "occurDate": 1621526400000,
            "customerName": "测试一号",
            "quotaBalanceAmount": 0,
            "termApplyType": "TT03",
            "applyNo": "appl20210521190742000039",
            "businessIs": "1",
            "wfStatus": "10",
            "businessType": "Y01",
            "contractSerialNo": "cont20210601112147000002"
        }]
    },
    "permitPropertys": {}
}


def test_json(request):
    #time.sleep(2)
    return JsonResponse(json1, status=200)


json2 = {
    "message": "操作成功",
    "data": {
        "token": "OTVlOGUxMzEwOTNhNGVkNTJlZTE2Mjc1MjNiZDA4NDU="
    },
}


def test_token(request):
    return JsonResponse(json2, status=200)


def index(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        return render(request, 'mock/index.html')


# http://172.22.82.105:9000/api/customer/app/execute

data = {
    "globalId": "",
    "appId": "",
    "requestId": "",
    "serviceId": "PeerCustomerAuthService",
    "requestType": "SMARTFORM",
    "action": "query",
    "parameters": {
        "filters": {
            "__EQS_customerType": "01"
        },
        "page": {
            "pageSize": 10,
            "pageNumber": 1,
            "pageable": 'true'
        },
        "orders": {}
    }
}


