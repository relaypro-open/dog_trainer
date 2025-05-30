# Documentation for Dog Trainer API

<a name="documentation-for-api-endpoints"></a>
## Documentation for API Endpoints

All URIs are relative to *http://localhost:7070*

| Class | Method | HTTP request | Description |
|------------ | ------------- | ------------- | -------------|
| *ExternalV1Api* | [**apiExternalIdDelete**](Apis/ExternalV1Api.md#apiexternaliddelete) | **DELETE** /api/external/{id} | Delete External (V1) |
*ExternalV1Api* | [**apiExternalIdGet**](Apis/ExternalV1Api.md#apiexternalidget) | **GET** /api/external/{id} | Get External by ID (V1) |
*ExternalV1Api* | [**apiExternalIdPut**](Apis/ExternalV1Api.md#apiexternalidput) | **PUT** /api/external/{id} | Update External (V1) |
*ExternalV1Api* | [**apiExternalIdSubGet**](Apis/ExternalV1Api.md#apiexternalidsubget) | **GET** /api/external/{id}/{sub} | Get External sub-resource (V1) |
*ExternalV1Api* | [**apiExternalPost**](Apis/ExternalV1Api.md#apiexternalpost) | **POST** /api/external | Create External (V1) |
*ExternalV1Api* | [**apiExternalsGet**](Apis/ExternalV1Api.md#apiexternalsget) | **GET** /api/externals | List Externals (V1) |
*ExternalV1Api* | [**apiExternalsSubGet**](Apis/ExternalV1Api.md#apiexternalssubget) | **GET** /api/externals/{sub} | List External sub-resources (V1) |
| *ExternalV2Api* | [**apiV2ExternalIdDelete**](Apis/ExternalV2Api.md#apiv2externaliddelete) | **DELETE** /api/V2/external/{id} | Delete an External resource by ID |
*ExternalV2Api* | [**apiV2ExternalIdGet**](Apis/ExternalV2Api.md#apiv2externalidget) | **GET** /api/V2/external/{id} | Get an External resource by ID |
*ExternalV2Api* | [**apiV2ExternalIdHclGet**](Apis/ExternalV2Api.md#apiv2externalidhclget) | **GET** /api/V2/external/{id}/hcl | Get HCL representation of an External resource |
*ExternalV2Api* | [**apiV2ExternalIdPut**](Apis/ExternalV2Api.md#apiv2externalidput) | **PUT** /api/V2/external/{id} | Update/Replace an External resource by ID |
*ExternalV2Api* | [**apiV2ExternalPost**](Apis/ExternalV2Api.md#apiv2externalpost) | **POST** /api/V2/external | Create a new External resource |
*ExternalV2Api* | [**apiV2ExternalsGet**](Apis/ExternalV2Api.md#apiv2externalsget) | **GET** /api/V2/externals | List all External resources |
*ExternalV2Api* | [**apiV2ExternalsSubGet**](Apis/ExternalV2Api.md#apiv2externalssubget) | **GET** /api/V2/externals/{sub} | Get a subset or filtered list of External resources (e.g., active) |
| *FactV1Api* | [**apiFactIdDelete**](Apis/FactV1Api.md#apifactiddelete) | **DELETE** /api/fact/{id} | Delete Fact (V1) |
*FactV1Api* | [**apiFactIdGet**](Apis/FactV1Api.md#apifactidget) | **GET** /api/fact/{id} | Get Fact by ID (V1) |
*FactV1Api* | [**apiFactIdPut**](Apis/FactV1Api.md#apifactidput) | **PUT** /api/fact/{id} | Update Fact (V1) |
*FactV1Api* | [**apiFactIdSubGet**](Apis/FactV1Api.md#apifactidsubget) | **GET** /api/fact/{id}/{sub} | Get Fact sub-resource (V1) |
*FactV1Api* | [**apiFactPost**](Apis/FactV1Api.md#apifactpost) | **POST** /api/fact | Create Fact (V1) |
*FactV1Api* | [**apiFactsGet**](Apis/FactV1Api.md#apifactsget) | **GET** /api/facts | List Facts (V1) |
*FactV1Api* | [**apiFactsSubGet**](Apis/FactV1Api.md#apifactssubget) | **GET** /api/facts/{sub} | List Fact sub-resources (V1) |
| *FactV2Api* | [**apiV2FactIdDelete**](Apis/FactV2Api.md#apiv2factiddelete) | **DELETE** /api/V2/fact/{id} | Delete a Fact by ID |
*FactV2Api* | [**apiV2FactIdGet**](Apis/FactV2Api.md#apiv2factidget) | **GET** /api/V2/fact/{id} | Get a Fact by ID |
*FactV2Api* | [**apiV2FactIdHclGet**](Apis/FactV2Api.md#apiv2factidhclget) | **GET** /api/V2/fact/{id}/hcl | Get HCL representation of a Fact |
*FactV2Api* | [**apiV2FactIdPut**](Apis/FactV2Api.md#apiv2factidput) | **PUT** /api/V2/fact/{id} | Update a Fact by ID |
*FactV2Api* | [**apiV2FactPost**](Apis/FactV2Api.md#apiv2factpost) | **POST** /api/V2/fact | Create a new Fact |
*FactV2Api* | [**apiV2FactsGet**](Apis/FactV2Api.md#apiv2factsget) | **GET** /api/V2/facts | List all Facts |
*FactV2Api* | [**apiV2FactsSubGet**](Apis/FactV2Api.md#apiv2factssubget) | **GET** /api/V2/facts/{sub} | Get a subset or filtered list of Facts (e.g. 'schema') |
| *FileTransferV2Api* | [**apiV2FileTransferIdDelete**](Apis/FileTransferV2Api.md#apiv2filetransferiddelete) | **DELETE** /api/V2/file_transfer/{id} | Delete a file on a host |
*FileTransferV2Api* | [**apiV2FileTransferIdGet**](Apis/FileTransferV2Api.md#apiv2filetransferidget) | **GET** /api/V2/file_transfer/{id} | Fetch a file from a host |
*FileTransferV2Api* | [**apiV2FileTransferIdPost**](Apis/FileTransferV2Api.md#apiv2filetransferidpost) | **POST** /api/V2/file_transfer/{id} | Execute a command or upload a file to a host |
| *GroupV1Api* | [**apiGroupIdDelete**](Apis/GroupV1Api.md#apigroupiddelete) | **DELETE** /api/group/{id} | Delete Group (V1) |
*GroupV1Api* | [**apiGroupIdGet**](Apis/GroupV1Api.md#apigroupidget) | **GET** /api/group/{id} | Get Group by ID (V1) |
*GroupV1Api* | [**apiGroupIdPut**](Apis/GroupV1Api.md#apigroupidput) | **PUT** /api/group/{id} | Update Group (V1) |
*GroupV1Api* | [**apiGroupIdSubGet**](Apis/GroupV1Api.md#apigroupidsubget) | **GET** /api/group/{id}/{sub} | Get Group sub-resource (V1) |
*GroupV1Api* | [**apiGroupPost**](Apis/GroupV1Api.md#apigrouppost) | **POST** /api/group | Create Group (V1) |
*GroupV1Api* | [**apiGroupsGet**](Apis/GroupV1Api.md#apigroupsget) | **GET** /api/groups | List Groups (V1) |
*GroupV1Api* | [**apiGroupsSubGet**](Apis/GroupV1Api.md#apigroupssubget) | **GET** /api/groups/{sub} | List Group sub-resources (V1) |
| *GroupV2Api* | [**apiV2GroupIdDelete**](Apis/GroupV2Api.md#apiv2groupiddelete) | **DELETE** /api/V2/group/{id} | Delete a Group by ID |
*GroupV2Api* | [**apiV2GroupIdEc2SecurityGroupIdsGet**](Apis/GroupV2Api.md#apiv2groupidec2securitygroupidsget) | **GET** /api/V2/group/{id}/ec2_security_group_ids | Get EC2 security group IDs for a Group |
*GroupV2Api* | [**apiV2GroupIdExternalIpsGet**](Apis/GroupV2Api.md#apiv2groupidexternalipsget) | **GET** /api/V2/group/{id}/external_ips | Get external IP addresses for a Group |
*GroupV2Api* | [**apiV2GroupIdGet**](Apis/GroupV2Api.md#apiv2groupidget) | **GET** /api/V2/group/{id} | Get a Group by ID |
*GroupV2Api* | [**apiV2GroupIdHclGet**](Apis/GroupV2Api.md#apiv2groupidhclget) | **GET** /api/V2/group/{id}/hcl | Get HCL representation of a Group |
*GroupV2Api* | [**apiV2GroupIdHostsGet**](Apis/GroupV2Api.md#apiv2groupidhostsget) | **GET** /api/V2/group/{id}/hosts | Get all Hosts within a Group |
*GroupV2Api* | [**apiV2GroupIdInternalIpsGet**](Apis/GroupV2Api.md#apiv2groupidinternalipsget) | **GET** /api/V2/group/{id}/internal_ips | Get internal IP addresses for a Group |
*GroupV2Api* | [**apiV2GroupIdIpsGet**](Apis/GroupV2Api.md#apiv2groupidipsget) | **GET** /api/V2/group/{id}/ips | Get all IP addresses for a Group |
*GroupV2Api* | [**apiV2GroupIdIpv4sGet**](Apis/GroupV2Api.md#apiv2groupidipv4sget) | **GET** /api/V2/group/{id}/ipv4s | Get all IPv4 addresses for a Group |
*GroupV2Api* | [**apiV2GroupIdIpv6sGet**](Apis/GroupV2Api.md#apiv2groupidipv6sget) | **GET** /api/V2/group/{id}/ipv6s | Get all IPv6 addresses for a Group |
*GroupV2Api* | [**apiV2GroupIdPut**](Apis/GroupV2Api.md#apiv2groupidput) | **PUT** /api/V2/group/{id} | Update a Group by ID |
*GroupV2Api* | [**apiV2GroupPost**](Apis/GroupV2Api.md#apiv2grouppost) | **POST** /api/V2/group | Create a new Group |
*GroupV2Api* | [**apiV2GroupsGet**](Apis/GroupV2Api.md#apiv2groupsget) | **GET** /api/V2/groups | List all Groups |
*GroupV2Api* | [**apiV2GroupsSubGet**](Apis/GroupV2Api.md#apiv2groupssubget) | **GET** /api/V2/groups/{sub} | Get a subset of Groups (e.g., 'active', 'all_active_names', 'names', 'schema') |
| *HCLApi* | [**apiHclSubGet**](Apis/HCLApi.md#apihclsubget) | **GET** /api/hcl/{sub} | Generate HCL for a given resource type |
| *HealthcheckApi* | [**apiHealthcheckGet**](Apis/HealthcheckApi.md#apihealthcheckget) | **GET** /api/healthcheck | Perform a health check of the API |
| *HostV1Api* | [**apiHostIdDelete**](Apis/HostV1Api.md#apihostiddelete) | **DELETE** /api/host/{id} | Delete Host (V1) |
*HostV1Api* | [**apiHostIdGet**](Apis/HostV1Api.md#apihostidget) | **GET** /api/host/{id} | Get Host by ID (V1) |
*HostV1Api* | [**apiHostIdPut**](Apis/HostV1Api.md#apihostidput) | **PUT** /api/host/{id} | Update Host (V1) |
*HostV1Api* | [**apiHostIdSubGet**](Apis/HostV1Api.md#apihostidsubget) | **GET** /api/host/{id}/{sub} | Get Host sub-resource (V1) |
*HostV1Api* | [**apiHostPost**](Apis/HostV1Api.md#apihostpost) | **POST** /api/host | Create Host (V1) |
*HostV1Api* | [**apiHostsGet**](Apis/HostV1Api.md#apihostsget) | **GET** /api/hosts | List Hosts (V1) |
*HostV1Api* | [**apiHostsIpsGet**](Apis/HostV1Api.md#apihostsipsget) | **GET** /api/hosts/ips | List Host IPs (V1) |
*HostV1Api* | [**apiHostsSubGet**](Apis/HostV1Api.md#apihostssubget) | **GET** /api/hosts/{sub} | List Host sub-resources (V1, e.g. 'ips') |
| *HostV2Api* | [**apiV2HostIdDelete**](Apis/HostV2Api.md#apiv2hostiddelete) | **DELETE** /api/V2/host/{id} | Delete a Host by ID |
*HostV2Api* | [**apiV2HostIdGet**](Apis/HostV2Api.md#apiv2hostidget) | **GET** /api/V2/host/{id} | Get a Host by ID |
*HostV2Api* | [**apiV2HostIdHclGet**](Apis/HostV2Api.md#apiv2hostidhclget) | **GET** /api/V2/host/{id}/hcl | Get HCL representation of a Host |
*HostV2Api* | [**apiV2HostIdPut**](Apis/HostV2Api.md#apiv2hostidput) | **PUT** /api/V2/host/{id} | Update a Host by ID |
*HostV2Api* | [**apiV2HostPost**](Apis/HostV2Api.md#apiv2hostpost) | **POST** /api/V2/host | Create a new Host |
*HostV2Api* | [**apiV2HostsGet**](Apis/HostV2Api.md#apiv2hostsget) | **GET** /api/V2/hosts | List all Hosts |
*HostV2Api* | [**apiV2HostsIpsGet**](Apis/HostV2Api.md#apiv2hostsipsget) | **GET** /api/V2/hosts/ips | Get all IPs for all active hosts (special plural handler case) |
*HostV2Api* | [**apiV2HostsSubGet**](Apis/HostV2Api.md#apiv2hostssubget) | **GET** /api/V2/hosts/{sub} | Get a subset of Hosts (e.g., 'active', 'ips', 'names', 'hostkeys', 'schema') |
| *IpsetsV2Api* | [**apiV2IpsetsGet**](Apis/IpsetsV2Api.md#apiv2ipsetsget) | **GET** /api/V2/ipsets | Get all IPset information |
| *LinkV1Api* | [**apiLinkIdDelete**](Apis/LinkV1Api.md#apilinkiddelete) | **DELETE** /api/link/{id} | Delete Link (V1) |
*LinkV1Api* | [**apiLinkIdGet**](Apis/LinkV1Api.md#apilinkidget) | **GET** /api/link/{id} | Get Link by ID (V1) |
*LinkV1Api* | [**apiLinkIdPut**](Apis/LinkV1Api.md#apilinkidput) | **PUT** /api/link/{id} | Update Link (V1) |
*LinkV1Api* | [**apiLinkIdSubGet**](Apis/LinkV1Api.md#apilinkidsubget) | **GET** /api/link/{id}/{sub} | Get Link sub-resource (V1) |
*LinkV1Api* | [**apiLinkPost**](Apis/LinkV1Api.md#apilinkpost) | **POST** /api/link | Create Link (V1) |
*LinkV1Api* | [**apiLinksGet**](Apis/LinkV1Api.md#apilinksget) | **GET** /api/links | List Links (V1) |
*LinkV1Api* | [**apiLinksSubGet**](Apis/LinkV1Api.md#apilinkssubget) | **GET** /api/links/{sub} | List Link sub-resources (V1) |
| *LinkV2Api* | [**apiV2LinkIdDelete**](Apis/LinkV2Api.md#apiv2linkiddelete) | **DELETE** /api/V2/link/{id} | Delete a Link by ID |
*LinkV2Api* | [**apiV2LinkIdGet**](Apis/LinkV2Api.md#apiv2linkidget) | **GET** /api/V2/link/{id} | Get a Link by ID |
*LinkV2Api* | [**apiV2LinkIdHclGet**](Apis/LinkV2Api.md#apiv2linkidhclget) | **GET** /api/V2/link/{id}/hcl | Get HCL representation of a Link |
*LinkV2Api* | [**apiV2LinkIdPut**](Apis/LinkV2Api.md#apiv2linkidput) | **PUT** /api/V2/link/{id} | Update a Link by ID |
*LinkV2Api* | [**apiV2LinkPost**](Apis/LinkV2Api.md#apiv2linkpost) | **POST** /api/V2/link | Create a new Link |
*LinkV2Api* | [**apiV2LinksGet**](Apis/LinkV2Api.md#apiv2linksget) | **GET** /api/V2/links | List all Links |
*LinkV2Api* | [**apiV2LinksSubGet**](Apis/LinkV2Api.md#apiv2linkssubget) | **GET** /api/V2/links/{sub} | Get a subset or filtered list of Links (e.g. 'schema') |
| *ProfileV1Api* | [**apiProfileIdDelete**](Apis/ProfileV1Api.md#apiprofileiddelete) | **DELETE** /api/profile/{id} | Delete Profile (V1) |
*ProfileV1Api* | [**apiProfileIdGet**](Apis/ProfileV1Api.md#apiprofileidget) | **GET** /api/profile/{id} | Get Profile by ID (V1) |
*ProfileV1Api* | [**apiProfileIdPut**](Apis/ProfileV1Api.md#apiprofileidput) | **PUT** /api/profile/{id} | Update Profile (V1) |
*ProfileV1Api* | [**apiProfileIdSubGet**](Apis/ProfileV1Api.md#apiprofileidsubget) | **GET** /api/profile/{id}/{sub} | Get Profile sub-resource (V1) |
*ProfileV1Api* | [**apiProfilePost**](Apis/ProfileV1Api.md#apiprofilepost) | **POST** /api/profile | Create Profile (V1) |
*ProfileV1Api* | [**apiProfilesGet**](Apis/ProfileV1Api.md#apiprofilesget) | **GET** /api/profiles | List Profiles (V1) |
*ProfileV1Api* | [**apiProfilesSubGet**](Apis/ProfileV1Api.md#apiprofilessubget) | **GET** /api/profiles/{sub} | List Profile sub-resources (V1) |
| *ProfileV2Api* | [**apiV2ProfileIdDelete**](Apis/ProfileV2Api.md#apiv2profileiddelete) | **DELETE** /api/V2/profile/{id} | Delete a Profile by ID |
*ProfileV2Api* | [**apiV2ProfileIdGet**](Apis/ProfileV2Api.md#apiv2profileidget) | **GET** /api/V2/profile/{id} | Get a Profile by ID |
*ProfileV2Api* | [**apiV2ProfileIdHclGet**](Apis/ProfileV2Api.md#apiv2profileidhclget) | **GET** /api/V2/profile/{id}/hcl | Get HCL representation of a Profile |
*ProfileV2Api* | [**apiV2ProfileIdPut**](Apis/ProfileV2Api.md#apiv2profileidput) | **PUT** /api/V2/profile/{id} | Update a Profile by ID |
*ProfileV2Api* | [**apiV2ProfilePost**](Apis/ProfileV2Api.md#apiv2profilepost) | **POST** /api/V2/profile | Create a new Profile |
*ProfileV2Api* | [**apiV2ProfilesGet**](Apis/ProfileV2Api.md#apiv2profilesget) | **GET** /api/V2/profiles | List all Profiles |
*ProfileV2Api* | [**apiV2ProfilesSubGet**](Apis/ProfileV2Api.md#apiv2profilessubget) | **GET** /api/V2/profiles/{sub} | Get a subset of Profiles (e.g., 'active', 'names', 'schema') |
| *PublishApi* | [**apiPublishPost**](Apis/PublishApi.md#apipublishpost) | **POST** /api/publish | Publish changes (specific action, details TBD) |
| *RulesetV1Api* | [**apiRulesetIdDelete**](Apis/RulesetV1Api.md#apirulesetiddelete) | **DELETE** /api/ruleset/{id} | Delete Ruleset (V1) |
*RulesetV1Api* | [**apiRulesetIdGet**](Apis/RulesetV1Api.md#apirulesetidget) | **GET** /api/ruleset/{id} | Get Ruleset by ID (V1) |
*RulesetV1Api* | [**apiRulesetIdPut**](Apis/RulesetV1Api.md#apirulesetidput) | **PUT** /api/ruleset/{id} | Update Ruleset (V1) |
*RulesetV1Api* | [**apiRulesetIdSubGet**](Apis/RulesetV1Api.md#apirulesetidsubget) | **GET** /api/ruleset/{id}/{sub} | Get Ruleset sub-resource (V1) |
*RulesetV1Api* | [**apiRulesetPost**](Apis/RulesetV1Api.md#apirulesetpost) | **POST** /api/ruleset | Create Ruleset (V1) |
*RulesetV1Api* | [**apiRulesetsGet**](Apis/RulesetV1Api.md#apirulesetsget) | **GET** /api/rulesets | List Rulesets (V1) |
*RulesetV1Api* | [**apiRulesetsSubGet**](Apis/RulesetV1Api.md#apirulesetssubget) | **GET** /api/rulesets/{sub} | List Ruleset sub-resources (V1) |
| *RulesetV2Api* | [**apiV2RulesetIdDelete**](Apis/RulesetV2Api.md#apiv2rulesetiddelete) | **DELETE** /api/V2/ruleset/{id} | Delete a Ruleset by ID |
*RulesetV2Api* | [**apiV2RulesetIdGet**](Apis/RulesetV2Api.md#apiv2rulesetidget) | **GET** /api/V2/ruleset/{id} | Get a Ruleset by ID |
*RulesetV2Api* | [**apiV2RulesetIdHclGet**](Apis/RulesetV2Api.md#apiv2rulesetidhclget) | **GET** /api/V2/ruleset/{id}/hcl | Get HCL representation of a Ruleset |
*RulesetV2Api* | [**apiV2RulesetIdPut**](Apis/RulesetV2Api.md#apiv2rulesetidput) | **PUT** /api/V2/ruleset/{id} | Update a Ruleset by ID |
*RulesetV2Api* | [**apiV2RulesetPost**](Apis/RulesetV2Api.md#apiv2rulesetpost) | **POST** /api/V2/ruleset | Create a new Ruleset |
*RulesetV2Api* | [**apiV2RulesetsGet**](Apis/RulesetV2Api.md#apiv2rulesetsget) | **GET** /api/V2/rulesets | List all Rulesets |
*RulesetV2Api* | [**apiV2RulesetsSubGet**](Apis/RulesetV2Api.md#apiv2rulesetssubget) | **GET** /api/V2/rulesets/{sub} | Get a subset of Rulesets (e.g., 'active', 'all_active_names', 'names', 'schema') |
| *ServiceV1Api* | [**apiServiceIdDelete**](Apis/ServiceV1Api.md#apiserviceiddelete) | **DELETE** /api/service/{id} | Delete Service (V1) |
*ServiceV1Api* | [**apiServiceIdGet**](Apis/ServiceV1Api.md#apiserviceidget) | **GET** /api/service/{id} | Get Service by ID (V1) |
*ServiceV1Api* | [**apiServiceIdPut**](Apis/ServiceV1Api.md#apiserviceidput) | **PUT** /api/service/{id} | Update Service (V1) |
*ServiceV1Api* | [**apiServiceIdSubGet**](Apis/ServiceV1Api.md#apiserviceidsubget) | **GET** /api/service/{id}/{sub} | Get Service sub-resource (V1) |
*ServiceV1Api* | [**apiServicePost**](Apis/ServiceV1Api.md#apiservicepost) | **POST** /api/service | Create Service (V1) |
*ServiceV1Api* | [**apiServicesGet**](Apis/ServiceV1Api.md#apiservicesget) | **GET** /api/services | List Services (V1) |
*ServiceV1Api* | [**apiServicesSubGet**](Apis/ServiceV1Api.md#apiservicessubget) | **GET** /api/services/{sub} | List Service sub-resources (V1) |
| *ServiceV2Api* | [**apiV2ServiceIdDelete**](Apis/ServiceV2Api.md#apiv2serviceiddelete) | **DELETE** /api/V2/service/{id} | Delete a Service by ID |
*ServiceV2Api* | [**apiV2ServiceIdGet**](Apis/ServiceV2Api.md#apiv2serviceidget) | **GET** /api/V2/service/{id} | Get a Service by ID |
*ServiceV2Api* | [**apiV2ServiceIdHclGet**](Apis/ServiceV2Api.md#apiv2serviceidhclget) | **GET** /api/V2/service/{id}/hcl | Get HCL representation of a Service |
*ServiceV2Api* | [**apiV2ServiceIdPut**](Apis/ServiceV2Api.md#apiv2serviceidput) | **PUT** /api/V2/service/{id} | Update a Service by ID |
*ServiceV2Api* | [**apiV2ServicePost**](Apis/ServiceV2Api.md#apiv2servicepost) | **POST** /api/V2/service | Create a new Service |
*ServiceV2Api* | [**apiV2ServicesGet**](Apis/ServiceV2Api.md#apiv2servicesget) | **GET** /api/V2/services | List all Services |
*ServiceV2Api* | [**apiV2ServicesSubGet**](Apis/ServiceV2Api.md#apiv2servicessubget) | **GET** /api/V2/services/{sub} | Get a subset or filtered list of Services (e.g. 'schema') |
| *ZoneV1Api* | [**apiZoneIdDelete**](Apis/ZoneV1Api.md#apizoneiddelete) | **DELETE** /api/zone/{id} | Delete Zone (V1) |
*ZoneV1Api* | [**apiZoneIdGet**](Apis/ZoneV1Api.md#apizoneidget) | **GET** /api/zone/{id} | Get Zone by ID (V1) |
*ZoneV1Api* | [**apiZoneIdPut**](Apis/ZoneV1Api.md#apizoneidput) | **PUT** /api/zone/{id} | Update Zone (V1) |
*ZoneV1Api* | [**apiZoneIdSubGet**](Apis/ZoneV1Api.md#apizoneidsubget) | **GET** /api/zone/{id}/{sub} | Get Zone sub-resource (V1) |
*ZoneV1Api* | [**apiZonePost**](Apis/ZoneV1Api.md#apizonepost) | **POST** /api/zone/ | Create Zone (V1) |
*ZoneV1Api* | [**apiZonesGet**](Apis/ZoneV1Api.md#apizonesget) | **GET** /api/zones | List Zones (V1) |
*ZoneV1Api* | [**apiZonesIpsGet**](Apis/ZoneV1Api.md#apizonesipsget) | **GET** /api/zones/ips | List Zone IPs (V1) |
*ZoneV1Api* | [**apiZonesSubGet**](Apis/ZoneV1Api.md#apizonessubget) | **GET** /api/zones/{sub} | List Zone sub-resources (V1, e.g. 'ips') |
| *ZoneV2Api* | [**apiV2ZoneIdDelete**](Apis/ZoneV2Api.md#apiv2zoneiddelete) | **DELETE** /api/V2/zone/{id} | Delete a Zone by ID |
*ZoneV2Api* | [**apiV2ZoneIdGet**](Apis/ZoneV2Api.md#apiv2zoneidget) | **GET** /api/V2/zone/{id} | Get a Zone by ID |
*ZoneV2Api* | [**apiV2ZoneIdHclGet**](Apis/ZoneV2Api.md#apiv2zoneidhclget) | **GET** /api/V2/zone/{id}/hcl | Get HCL representation of a Zone |
*ZoneV2Api* | [**apiV2ZoneIdPut**](Apis/ZoneV2Api.md#apiv2zoneidput) | **PUT** /api/V2/zone/{id} | Update a Zone by ID |
*ZoneV2Api* | [**apiV2ZonePost**](Apis/ZoneV2Api.md#apiv2zonepost) | **POST** /api/V2/zone/ | Create a new Zone |
*ZoneV2Api* | [**apiV2ZonesGet**](Apis/ZoneV2Api.md#apiv2zonesget) | **GET** /api/V2/zones | List all Zones |
*ZoneV2Api* | [**apiV2ZonesIpsGet**](Apis/ZoneV2Api.md#apiv2zonesipsget) | **GET** /api/V2/zones/ips | Get all IPs for all zones (special plural handler case) |
*ZoneV2Api* | [**apiV2ZonesSubGet**](Apis/ZoneV2Api.md#apiv2zonessubget) | **GET** /api/V2/zones/{sub} | Get a subset of Zones (e.g., 'ips', 'names', 'schema') |


<a name="documentation-for-models"></a>
## Documentation for Models

 - [AnyRule](./Models/AnyRule.md)
 - [Ec2SecurityGroup](./Models/Ec2SecurityGroup.md)
 - [Error](./Models/Error.md)
 - [External](./Models/External.md)
 - [ExternalVersion](./Models/ExternalVersion.md)
 - [Fact](./Models/Fact.md)
 - [FactGroupDefinition](./Models/FactGroupDefinition.md)
 - [FileTransferCommand](./Models/FileTransferCommand.md)
 - [FileTransferCommandResponse_value](./Models/FileTransferCommandResponse_value.md)
 - [FileTransferUploadResponseItem](./Models/FileTransferUploadResponseItem.md)
 - [Group](./Models/Group.md)
 - [HealthcheckResponse](./Models/HealthcheckResponse.md)
 - [Host](./Models/Host.md)
 - [IpsetInfo](./Models/IpsetInfo.md)
 - [Link](./Models/Link.md)
 - [LinkConnection](./Models/LinkConnection.md)
 - [LinkSslOptions](./Models/LinkSslOptions.md)
 - [Profile](./Models/Profile.md)
 - [RuleBasic](./Models/RuleBasic.md)
 - [RuleConnlimit](./Models/RuleConnlimit.md)
 - [RuleProperties](./Models/RuleProperties.md)
 - [RuleRecent](./Models/RuleRecent.md)
 - [Ruleset](./Models/Ruleset.md)
 - [Ruleset_rules](./Models/Ruleset_rules.md)
 - [Service](./Models/Service.md)
 - [ServiceItem](./Models/ServiceItem.md)
 - [ServiceItem_protocol](./Models/ServiceItem_protocol.md)
 - [Zone](./Models/Zone.md)
 - [_api_V2_file_transfer__id__post_200_response](./Models/_api_V2_file_transfer__id__post_200_response.md)
 - [_api_V2_groups__sub__get_200_response_inner](./Models/_api_V2_groups__sub__get_200_response_inner.md)
 - [_api_V2_hosts__sub__get_200_response_inner](./Models/_api_V2_hosts__sub__get_200_response_inner.md)
 - [_api_V2_profiles__sub__get_200_response_inner](./Models/_api_V2_profiles__sub__get_200_response_inner.md)
 - [_api_V2_rulesets__sub__get_200_response_inner](./Models/_api_V2_rulesets__sub__get_200_response_inner.md)
 - [_api_V2_zones__sub__get_200_response_inner](./Models/_api_V2_zones__sub__get_200_response_inner.md)


<a name="documentation-for-authorization"></a>
## Documentation for Authorization

<a name="KongApiKey"></a>
### KongApiKey

- **Type**: API key
- **API key parameter name**: apikey
- **Location**: HTTP header

<a name="ConsumerInfo"></a>
### ConsumerInfo

- **Type**: API key
- **API key parameter name**: X-Consumer-Username
- **Location**: HTTP header

