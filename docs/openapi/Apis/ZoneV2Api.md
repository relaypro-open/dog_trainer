# ZoneV2Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiV2ZoneIdDelete**](ZoneV2Api.md#apiV2ZoneIdDelete) | **DELETE** /api/V2/zone/{id} | Delete a Zone by ID |
| [**apiV2ZoneIdGet**](ZoneV2Api.md#apiV2ZoneIdGet) | **GET** /api/V2/zone/{id} | Get a Zone by ID |
| [**apiV2ZoneIdHclGet**](ZoneV2Api.md#apiV2ZoneIdHclGet) | **GET** /api/V2/zone/{id}/hcl | Get HCL representation of a Zone |
| [**apiV2ZoneIdPut**](ZoneV2Api.md#apiV2ZoneIdPut) | **PUT** /api/V2/zone/{id} | Update a Zone by ID |
| [**apiV2ZonePost**](ZoneV2Api.md#apiV2ZonePost) | **POST** /api/V2/zone/ | Create a new Zone |
| [**apiV2ZonesGet**](ZoneV2Api.md#apiV2ZonesGet) | **GET** /api/V2/zones | List all Zones |
| [**apiV2ZonesIpsGet**](ZoneV2Api.md#apiV2ZonesIpsGet) | **GET** /api/V2/zones/ips | Get all IPs for all zones (special plural handler case) |
| [**apiV2ZonesSubGet**](ZoneV2Api.md#apiV2ZonesSubGet) | **GET** /api/V2/zones/{sub} | Get a subset of Zones (e.g., &#39;ips&#39;, &#39;names&#39;, &#39;schema&#39;) |


<a name="apiV2ZoneIdDelete"></a>
# **apiV2ZoneIdDelete**
> apiV2ZoneIdDelete(id, inplace)

Delete a Zone by ID

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

<a name="apiV2ZoneIdGet"></a>
# **apiV2ZoneIdGet**
> Zone apiV2ZoneIdGet(id, inplace)

Get a Zone by ID

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **inplace** | **String**| For PUT operations, whether to update in place or create a new version (specific to some handlers) | [optional] [default to null] [enum: True, False] |

### Return type

[**Zone**](../Models/Zone.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2ZoneIdHclGet"></a>
# **apiV2ZoneIdHclGet**
> String apiV2ZoneIdHclGet(id)

Get HCL representation of a Zone

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

<a name="apiV2ZoneIdPut"></a>
# **apiV2ZoneIdPut**
> apiV2ZoneIdPut(id, Zone, inplace)

Update a Zone by ID

    The backend &#x60;dog_zone_api_v2:update/2&#x60; calls &#x60;reql:update&#x60;, implying a partial update is possible. &#x60;api_handler_v2&#x60; supports &#x60;inplace&#x60; query param, but &#x60;dog_zone_api_v2:update/2&#x60; doesn&#39;t use it.

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Zone** | [**Zone**](../Models/Zone.md)|  | |
| **inplace** | **String**| For PUT operations, whether to update in place or create a new version (specific to some handlers) | [optional] [default to null] [enum: True, False] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2ZonePost"></a>
# **apiV2ZonePost**
> Zone apiV2ZonePost(Zone)

Create a new Zone

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Zone** | [**Zone**](../Models/Zone.md)|  | |

### Return type

[**Zone**](../Models/Zone.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiV2ZonesGet"></a>
# **apiV2ZonesGet**
> List apiV2ZonesGet()

List all Zones

### Parameters
This endpoint does not need any parameter.

### Return type

[**List**](../Models/Zone.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2ZonesIpsGet"></a>
# **apiV2ZonesIpsGet**
> Map apiV2ZonesIpsGet()

Get all IPs for all zones (special plural handler case)

### Parameters
This endpoint does not need any parameter.

### Return type

[**Map**](../Models/array.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2ZonesSubGet"></a>
# **apiV2ZonesSubGet**
> List apiV2ZonesSubGet(sub)

Get a subset of Zones (e.g., &#39;ips&#39;, &#39;names&#39;, &#39;schema&#39;)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **sub** | **String**| Sub-resource or action | [default to null] |

### Return type

[**List**](../Models/_api_V2_zones__sub__get_200_response_inner.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json, text/plain

