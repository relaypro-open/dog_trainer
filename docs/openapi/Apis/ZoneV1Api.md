# ZoneV1Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiZoneIdDelete**](ZoneV1Api.md#apiZoneIdDelete) | **DELETE** /api/zone/{id} | Delete Zone (V1) |
| [**apiZoneIdGet**](ZoneV1Api.md#apiZoneIdGet) | **GET** /api/zone/{id} | Get Zone by ID (V1) |
| [**apiZoneIdPut**](ZoneV1Api.md#apiZoneIdPut) | **PUT** /api/zone/{id} | Update Zone (V1) |
| [**apiZoneIdSubGet**](ZoneV1Api.md#apiZoneIdSubGet) | **GET** /api/zone/{id}/{sub} | Get Zone sub-resource (V1) |
| [**apiZonePost**](ZoneV1Api.md#apiZonePost) | **POST** /api/zone/ | Create Zone (V1) |
| [**apiZonesGet**](ZoneV1Api.md#apiZonesGet) | **GET** /api/zones | List Zones (V1) |
| [**apiZonesIpsGet**](ZoneV1Api.md#apiZonesIpsGet) | **GET** /api/zones/ips | List Zone IPs (V1) |
| [**apiZonesSubGet**](ZoneV1Api.md#apiZonesSubGet) | **GET** /api/zones/{sub} | List Zone sub-resources (V1, e.g. &#39;ips&#39;) |


<a name="apiZoneIdDelete"></a>
# **apiZoneIdDelete**
> apiZoneIdDelete(id)

Delete Zone (V1)

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

<a name="apiZoneIdGet"></a>
# **apiZoneIdGet**
> apiZoneIdGet(id)

Get Zone by ID (V1)

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

<a name="apiZoneIdPut"></a>
# **apiZoneIdPut**
> apiZoneIdPut(id, Zone)

Update Zone (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Resource ID | [default to null] |
| **Zone** | [**Zone**](../Models/Zone.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiZoneIdSubGet"></a>
# **apiZoneIdSubGet**
> apiZoneIdSubGet(id, sub)

Get Zone sub-resource (V1)

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

<a name="apiZonePost"></a>
# **apiZonePost**
> apiZonePost(Zone)

Create Zone (V1)

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **Zone** | [**Zone**](../Models/Zone.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

<a name="apiZonesGet"></a>
# **apiZonesGet**
> apiZonesGet()

List Zones (V1)

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

<a name="apiZonesIpsGet"></a>
# **apiZonesIpsGet**
> apiZonesIpsGet()

List Zone IPs (V1)

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

<a name="apiZonesSubGet"></a>
# **apiZonesSubGet**
> apiZonesSubGet(sub)

List Zone sub-resources (V1, e.g. &#39;ips&#39;)

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

