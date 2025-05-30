# RulesetV2Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiV2RulesetIdDelete**](RulesetV2Api.md#apiV2RulesetIdDelete) | **DELETE** /api/V2/ruleset/{id} | Delete a Ruleset by ID |
| [**apiV2RulesetIdGet**](RulesetV2Api.md#apiV2RulesetIdGet) | **GET** /api/V2/ruleset/{id} | Get a Ruleset by ID |
| [**apiV2RulesetIdHclGet**](RulesetV2Api.md#apiV2RulesetIdHclGet) | **GET** /api/V2/ruleset/{id}/hcl | Get HCL representation of a Ruleset |
| [**apiV2RulesetIdPut**](RulesetV2Api.md#apiV2RulesetIdPut) | **PUT** /api/V2/ruleset/{id} | Update a Ruleset by ID |
| [**apiV2RulesetPost**](RulesetV2Api.md#apiV2RulesetPost) | **POST** /api/V2/ruleset | Create a new Ruleset |
| [**apiV2RulesetsGet**](RulesetV2Api.md#apiV2RulesetsGet) | **GET** /api/V2/rulesets | List all Rulesets |
| [**apiV2RulesetsSubGet**](RulesetV2Api.md#apiV2RulesetsSubGet) | **GET** /api/V2/rulesets/{sub} | Get a subset of Rulesets (e.g., &#39;active&#39;, &#39;all_active_names&#39;, &#39;names&#39;, &#39;schema&#39;) |


<a name="apiV2RulesetIdDelete"></a>
# **apiV2RulesetIdDelete**
> apiV2RulesetIdDelete(id, inplace)

Delete a Ruleset by ID

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **inplace** | **String**| For PUT operations, whether to update in place or create a new version (specific to some handlers) | [optional] [default to null] [enum: True, False] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2RulesetIdGet"></a>
# **apiV2RulesetIdGet**
> Ruleset apiV2RulesetIdGet(id, inplace)

Get a Ruleset by ID

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **inplace** | **String**| For PUT operations, whether to update in place or create a new version (specific to some handlers) | [optional] [default to null] [enum: True, False] |

### Return type

[**Ruleset**](../Models/Ruleset.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2RulesetIdHclGet"></a>
# **apiV2RulesetIdHclGet**
> String apiV2RulesetIdHclGet(id)

Get HCL representation of a Ruleset

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |

### Return type

**String**

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain, application/json

<a name="apiV2RulesetIdPut"></a>
# **apiV2RulesetIdPut**
> apiV2RulesetIdPut(id, Ruleset, inplace)

Update a Ruleset by ID

    The backend &#x60;dog_ruleset_api_v2:update/2&#x60; calls &#x60;reql:update&#x60;, implying a partial update is possible. &#x60;api_handler_v2&#x60; supports &#x60;inplace&#x60; query param, but &#x60;dog_ruleset_api_v2:update/2&#x60; doesn&#39;t use it.

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Ruleset** | [**Ruleset**](../Models/Ruleset.md)|  | |
| **inplace** | **String**| For PUT operations, whether to update in place or create a new version (specific to some handlers) | [optional] [default to null] [enum: True, False] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2RulesetPost"></a>
# **apiV2RulesetPost**
> Ruleset apiV2RulesetPost(Ruleset)

Create a new Ruleset

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Ruleset** | [**Ruleset**](../Models/Ruleset.md)|  | |

### Return type

[**Ruleset**](../Models/Ruleset.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2RulesetsGet"></a>
# **apiV2RulesetsGet**
> List apiV2RulesetsGet()

List all Rulesets

### Parameters
This endpoint does not need any parameter.

### Return type

[**List**](../Models/Ruleset.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2RulesetsSubGet"></a>
# **apiV2RulesetsSubGet**
> List apiV2RulesetsSubGet(sub)

Get a subset of Rulesets (e.g., &#39;active&#39;, &#39;all_active_names&#39;, &#39;names&#39;, &#39;schema&#39;)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **sub** | **String**| Sub-resource or action | [default to null] |

### Return type

[**List**](../Models/_api_V2_rulesets__sub__get_200_response_inner.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json, text/plain

