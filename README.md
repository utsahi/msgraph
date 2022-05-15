# msgraph
A set of libraries and tools to interact with the Microsoft Graph API

*msgraph.el* cotains 
1. Helpers to perform the HTTP operations against the MS Graph interface. 
2. A data structure to store authentication tokens for the given hosts. This data structure is used to construct the authorization header in the HTTP requests.

*msgraph-email.el* is a major mode to interact with the MSGraph email API.
To use it,
1. Acquire a token for use with the MS Graph API.
3. Set the token using (msgraph-set-auth-info "graph.microsoft.com" "...token-value...")
4. Browse email with *M-x msgraph-email*

Currently, sending emails is not implemented.
