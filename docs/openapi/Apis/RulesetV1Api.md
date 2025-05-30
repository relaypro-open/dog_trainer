# RulesetV1Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiRulesetIdDelete**](RulesetV1Api.md#apiRulesetIdDelete) | **DELETE** /api/ruleset/{id} | Delete Ruleset (V1) |
| [**apiRulesetIdGet**](RulesetV1Api.md#apiRulesetIdGet) | **GET** /api/ruleset/{id} | Get Ruleset by ID (V1) |
| [**apiRulesetIdPut**](RulesetV1Api.md#apiRulesetIdPut) | **PUT** /api/ruleset/{id} | Update Ruleset (V1) |
| [**apiRulesetIdSubGet**](RulesetV1Api.md#apiRulesetIdSubGet) | **GET** /api/ruleset/{id}/{sub} | Get Ruleset sub-resource (V1) |
| [**apiRulesetPost**](RulesetV1Api.md#apiRulesetPost) | **POST** /api/ruleset | Create Ruleset (V1) |
| [**apiRulesetsGet**](RulesetV1Api.md#apiRulesetsGet) | **GET** /api/rulesets | List Rulesets (V1) |
| [**apiRulesetsSubGet**](RulesetV1Api.md#apiRulesetsSubGet) | **GET** /api/rulesets/{sub} | List Ruleset sub-resources (V1) |


<a name="apiRulesetIdDelete"></a>
# **apiRulesetIdDelete**
> apiRulesetIdDelete(id)

Delete Ruleset (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiRulesetIdGet"></a>
# **apiRulesetIdGet**
> apiRulesetIdGet(id)

Get Ruleset by ID (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiRulesetIdPut"></a>
# **apiRulesetIdPut**
> apiRulesetIdPut(id, Ruleset)

Update Ruleset (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Ruleset** | [**Ruleset**](../Models/Ruleset.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiRulesetIdSubGet"></a>
# **apiRulesetIdSubGet**
> apiRulesetIdSubGet(id, sub)

Get Ruleset sub-resource (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **sub** | **String**| Sub-resource or action | [default to null] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiRulesetPost"></a>
# **apiRulesetPost**
> apiRulesetPost(Ruleset)

Create Ruleset (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Ruleset** | [**Ruleset**](../Models/Ruleset.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiRulesetsGet"></a>
# **apiRulesetsGet**
> apiRulesetsGet()

List Rulesets (V1)

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

<a name="apiRulesetsSubGet"></a>
# **apiRulesetsSubGet**
> apiRulesetsSubGet(sub)

List Ruleset sub-resources (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **sub** | **String**| Sub-resource or action | [default to null] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

