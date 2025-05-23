# Azure Integration Landing Zone - COBOL Implementation

This directory contains a complete COBOL implementation for deploying Azure Integration Landing Zone resources using Azure Resource Manager (ARM) REST APIs. This implementation demonstrates how mainframe developers can leverage their existing COBOL skills to manage cloud infrastructure.

## 🚀 Overview

The Azure Integration Landing Zone COBOL implementation provides a mainframe-style approach to cloud resource deployment. Instead of learning new domain-specific languages like Terraform or ARM templates, mainframe developers can use familiar COBOL syntax and structures to deploy Azure resources.

### Why COBOL for Cloud Infrastructure?

- **Familiar Syntax**: Leverage existing COBOL skills for cloud deployment
- **Structured Programming**: COBOL's structured approach provides clear, maintainable infrastructure code
- **Enterprise Integration**: Seamlessly integrate cloud deployment with existing mainframe applications
- **Proven Reliability**: COBOL's reliability and error handling for mission-critical infrastructure
- **Copybook Reusability**: Share configuration structures across multiple deployment scenarios

## 📁 Directory Structure

```
infra/cobol/
├── src/                    # Main COBOL source programs
│   ├── AZUREDEPLOY.cbl    # Main deployment orchestrator
│   ├── AZUREAUTH.cbl      # Azure authentication module
│   ├── STORAGE.cbl        # Storage Account deployment
│   ├── KEYVAULT.cbl       # Key Vault deployment
│   ├── VIRTUALNET.cbl     # Virtual Network deployment
│   └── LOGANALYTICS.cbl   # Log Analytics Workspace deployment
├── copybooks/              # COBOL copybooks for data structures
│   ├── AZURECONFIG.cpy    # Azure configuration data structure
│   └── HTTPCLIENT.cpy     # HTTP client data structures
├── config/                 # Configuration files
│   └── azure-config.conf  # Azure deployment configuration
├── tests/                  # Unit tests
│   ├── TESTAUTH.cbl       # Authentication module tests
│   ├── TESTSTORAGE.cbl    # Storage module tests
│   └── run-tests.sh       # Test runner script
└── doc/                    # Documentation (this README)
```

## 🔧 Prerequisites

### Software Requirements

1. **COBOL Compiler**: GnuCOBOL (formerly OpenCOBOL) 3.x or later
   ```bash
   # Ubuntu/Debian
   sudo apt-get install gnucobol
   
   # CentOS/RHEL
   sudo yum install gnucobol
   
   # macOS
   brew install gnu-cobol
   ```

2. **HTTP Client Library**: For making REST API calls
   - libcurl-dev (for HTTP operations)
   - JSON parsing library (optional, basic parsing included)

3. **Azure Prerequisites**:
   - Azure subscription
   - Service principal with appropriate permissions
   - Resource group (existing)

### Azure Service Principal Setup

Create a service principal for authentication:

```bash
az ad sp create-for-rbac --name "cobol-deployment-sp" \
    --role "Contributor" \
    --scopes "/subscriptions/{subscription-id}"
```

Note the output values for configuration.

## ⚙️ Configuration

### 1. Update Configuration File

Edit `config/azure-config.conf` with your Azure details:

```
# Azure Subscription Information
SUBSCRIPTION_ID=your-subscription-id-here
TENANT_ID=your-tenant-id-here  
CLIENT_ID=your-client-id-here
CLIENT_SECRET=your-client-secret-here

# Resource Group Configuration
RESOURCE_GROUP_NAME=RG-AIS-LZ-TF
LOCATION=eastus2

# Environment Settings
ENVIRONMENT=dev
SUFFIX=lz-tf
```

**Important**: The configuration file uses fixed-width format suitable for COBOL parsing. Maintain the spacing as shown.

### 2. Configure Resources

The configuration file supports the following Azure resources:

#### Virtual Network
```
VNET_ADDRESS_SPACE=10.10.0.0/16
SUBNET_COUNT=03
SUBNET_001_NAME=ase
SUBNET_001_PREFIX=10.10.1.0/24
SUBNET_002_NAME=private-endpoints  
SUBNET_002_PREFIX=10.10.2.0/24
SUBNET_003_NAME=apim
SUBNET_003_PREFIX=10.10.3.0/24
```

#### Storage Accounts
```
STORAGE_COUNT=01
STORAGE_001_PREFIX=apimbackup
STORAGE_001_SKU=Standard_LRS
STORAGE_001_KIND=StorageV2
STORAGE_001_TIER=Hot
```

#### Key Vault
```
KEY_VAULT_PURGE_PROTECTION=Y
KEY_VAULT_SOFT_DELETE_DAYS=007
```

#### API Management (Optional)
```
APIM_DEPLOY=Y
APIM_SKU_NAME=Developer
APIM_SKU_CAPACITY=01
APIM_PUBLISHER_NAME=Contoso
APIM_PUBLISHER_EMAIL=apis@contoso.net
```

## 🏗️ Building and Running

### 1. Compile the Programs

```bash
# Navigate to the COBOL directory
cd infra/cobol

# Compile main programs
cobc -x -I copybooks -o bin/azuredeploy src/AZUREDEPLOY.cbl
cobc -x -I copybooks -o bin/azureauth src/AZUREAUTH.cbl
cobc -x -I copybooks -o bin/storage src/STORAGE.cbl
cobc -x -I copybooks -o bin/keyvault src/KEYVAULT.cbl
cobc -x -I copybooks -o bin/virtualnet src/VIRTUALNET.cbl
cobc -x -I copybooks -o bin/loganalytics src/LOGANALYTICS.cbl
```

### 2. Run the Deployment

```bash
# Execute the main deployment program
./bin/azuredeploy
```

### 3. Run Unit Tests

```bash
# Run all unit tests
cd tests
./run-tests.sh
```

## 📋 Program Descriptions

### AZUREDEPLOY.cbl (Main Orchestrator)
- **Purpose**: Main deployment coordinator
- **Function**: Orchestrates the deployment of all Azure resources in dependency order
- **Features**:
  - Dependency management (deploys Log Analytics first, then VNet, etc.)
  - Comprehensive logging
  - Error handling and rollback
  - Progress tracking

### AZUREAUTH.cbl (Authentication Module)
- **Purpose**: Handles Azure Active Directory authentication
- **Function**: Obtains access tokens for ARM API calls
- **Features**:
  - Service principal authentication
  - Token caching and renewal
  - Secure credential handling

### Resource Deployment Modules

#### STORAGE.cbl
- Creates Azure Storage Accounts
- Supports multiple storage accounts with different configurations
- Implements naming conventions
- Configures security settings (private endpoints, TLS)

#### KEYVAULT.cbl
- Deploys Azure Key Vault
- Configures RBAC authorization
- Sets up soft delete and purge protection
- Implements private networking

#### VIRTUALNET.cbl
- Creates Virtual Networks and subnets
- Supports multiple subnets with different configurations
- Implements network security best practices

#### LOGANALYTICS.cbl
- Deploys Log Analytics Workspace
- Configures retention and capacity settings
- Sets up monitoring infrastructure

## 🔒 Security Considerations

### Authentication
- Uses Azure AD service principal authentication
- Supports credential rotation
- Implements secure token handling

### Network Security
- All resources deployed with private endpoints
- Public access disabled by default
- Network isolation implemented

### Configuration Security
- Configuration file should be secured with appropriate permissions
- Consider using Azure Key Vault for sensitive configuration
- Implement credential encryption in production

## 🧪 Testing

### Unit Test Coverage
- **TESTAUTH.cbl**: Tests authentication module
- **TESTSTORAGE.cbl**: Tests storage account deployment logic

### Test Execution
```bash
cd tests
./run-tests.sh
```

### Manual Testing
1. Verify configuration loading
2. Test authentication token acquisition
3. Validate ARM API URL construction
4. Check JSON payload generation

## 🔍 Troubleshooting

### Common Issues

#### Compilation Errors
```
Error: COPY book not found
Solution: Ensure copybooks directory is in the include path (-I copybooks)
```

#### Authentication Failures
```
Error: HTTP 401 Unauthorized
Solution: Verify service principal credentials and permissions
```

#### Resource Deployment Failures
```
Error: HTTP 400 Bad Request
Solution: Check JSON payload format and required parameters
```

### Debug Mode
Enable verbose logging by setting:
```cobol
01  DEBUG-MODE              PIC X VALUE 'Y'.
```

### Log Files
- Deployment log: `deployment.log`
- Error details in program output
- ARM API response details

## 🚀 Extending the Implementation

### Adding New Resource Types

1. **Create Resource Module**:
   ```cobol
   PROGRAM-ID. NEWRESOURCE.
   ```

2. **Update Main Deployment**:
   ```cobol
   PERFORM DEPLOY-NEW-RESOURCE
   ```

3. **Add Configuration**:
   ```
   NEW_RESOURCE_SETTING=value
   ```

### Custom Naming Conventions
Modify naming logic in each resource module:
```cobol
STRING 'custom-prefix-'
       ENVIRONMENT
       '-'
       SUFFIX
    DELIMITED BY SIZE INTO RESOURCE-NAME
END-STRING
```

### Additional HTTP Methods
Extend HTTPCLIENT.cpy for DELETE, PATCH operations:
```cobol
01  HTTP-DELETE-REQUEST.
    05  HTTP-METHOD         PIC X(10) VALUE 'DELETE'.
```

## 📚 Learning Resources

### COBOL Resources
- [GnuCOBOL Documentation](https://gnucobol.sourceforge.io/)
- [COBOL Programming Guide](https://www.ibm.com/docs/en/cobol-zos)

### Azure ARM API Resources
- [Azure REST API Reference](https://docs.microsoft.com/en-us/rest/api/azure/)
- [Azure Resource Manager Templates](https://docs.microsoft.com/en-us/azure/azure-resource-manager/templates/)

### Integration Examples
- [Mainframe to Cloud Migration Patterns](https://docs.microsoft.com/en-us/azure/architecture/reference-architectures/migration/mainframe-migration-overview)

## 🤝 Contributing

### Code Style
- Follow COBOL-85 standards
- Use descriptive variable names
- Include comprehensive comments
- Maintain copybook consistency

### Testing Requirements
- Add unit tests for new modules
- Update integration tests
- Validate error handling

### Documentation
- Update README for new features
- Document configuration options
- Include troubleshooting guides

## 📄 License

This implementation follows the same license as the parent Azure Integration Landing Zone project.

## 🙋‍♂️ Support

For issues and questions:
1. Check the troubleshooting section
2. Review the test results
3. Examine the deployment logs
4. Open an issue with detailed error information

---

*This COBOL implementation demonstrates that mainframe developers can effectively manage cloud infrastructure using familiar tools and patterns. The structured approach of COBOL provides excellent maintainability and reliability for infrastructure-as-code scenarios.*