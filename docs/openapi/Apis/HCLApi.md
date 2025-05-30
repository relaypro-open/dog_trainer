# HCLApi

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiHclSubGet**](HCLApi.md#apiHclSubGet) | **GET** /api/hcl/{sub} | Generate HCL for a given resource type |


<a name="apiHclSubGet"></a>
# **apiHclSubGet**
> String apiHclSubGet(sub)

Generate HCL for a given resource type

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **sub** | **String**| The type of resource to generate HCL for (e.g., &#39;group&#39;, &#39;host&#39;). | [default to null] |

### Return type

**String**

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain, application/json

