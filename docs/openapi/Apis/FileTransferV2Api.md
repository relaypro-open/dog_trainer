# FileTransferV2Api

All URIs are relative to *http://localhost:7070*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**apiV2FileTransferIdDelete**](FileTransferV2Api.md#apiV2FileTransferIdDelete) | **DELETE** /api/V2/file_transfer/{id} | Delete a file on a host |
| [**apiV2FileTransferIdGet**](FileTransferV2Api.md#apiV2FileTransferIdGet) | **GET** /api/V2/file_transfer/{id} | Fetch a file from a host |
| [**apiV2FileTransferIdPost**](FileTransferV2Api.md#apiV2FileTransferIdPost) | **POST** /api/V2/file_transfer/{id} | Execute a command or upload a file to a host |


<a name="apiV2FileTransferIdDelete"></a>
# **apiV2FileTransferIdDelete**
> Error apiV2FileTransferIdDelete(id, path)

Delete a file on a host

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Hostkey of the target host | [default to null] |
| **path** | **String**| Path to the file for transfer operations | [default to null] |

### Return type

[**Error**](../Models/Error.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

<a name="apiV2FileTransferIdGet"></a>
# **apiV2FileTransferIdGet**
> File apiV2FileTransferIdGet(id, path)

Fetch a file from a host

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Hostkey of the target host | [default to null] |
| **path** | **String**| Path to the file for transfer operations | [default to null] |

### Return type

**File**

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/octet-stream, application/json

<a name="apiV2FileTransferIdPost"></a>
# **apiV2FileTransferIdPost**
> _api_V2_file_transfer__id__post_200_response apiV2FileTransferIdPost(id, FileTransferCommand)

Execute a command or upload a file to a host

    Accepts &#39;application/json&#39; for command execution or &#39;multipart/form-data&#39; for file uploads.

### Parameters

|Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **id** | **String**| Hostkey of the target host | [default to null] |
| **FileTransferCommand** | [**FileTransferCommand**](../Models/FileTransferCommand.md)|  | [optional] |

### Return type

[**_api_V2_file_transfer__id__post_200_response**](../Models/_api_V2_file_transfer__id__post_200_response.md)

### Authorization

[ConsumerInfo](../README.md#ConsumerInfo), [KongApiKey](../README.md#KongApiKey)

### HTTP request headers

- **Content-Type**: application/json, multipart/form-data
- **Accept**: application/json

