# icleaned
icleaned is a Shiny app that calculates environmental footprints of livestock production systems

## Environment Configuration

The `.Renviron` file is essential for securely configuring sensitive environment variables for the Shiny app, such as API keys, directory paths, and analytics credentials. This file resides in the root directory of the project and contains variables such as:

* `APP_URL`: The base URL of the application, typically in the form `https://myapp.com`. This URL is where users access the Shiny app.

* `AUTH0_USER`: The Auth0 domain for the app, provided by Auth0, usually in the format `my-app.auth0.com`. This domain is the unique identifier for the Auth0 tenant and connects the app to the Auth0 account.

* `AUTH0_KEY`: The client ID generated in Auth0 for the specific application. This ID is a public identifier that the app uses to initiate authentication requests.

* `AUTH0_SECRET`: The client secret from Auth0, associated with the `AUTH0_KEY`. This is a private key that securely signs authentication tokens and must remain confidential.

* `AUTH0_ENABLED`: A boolean flag (`TRUE` or `FALSE`) that enables or disables Auth0 authentication, mainly for development purposes.

* `DATA_DIR`: The directory path where client data is securely stored, allowing the app to reference files or data specific to each client.

* `SC_PROJECT`, `SC_SECURITY`: Credentials for [StateCounter](https://statcounter.com/) analytics integration. These track and report user activity within the app for analytical purposes.

> **Note:** The `.Renviron` file contains sensitive information, including API keys and analytics credentials. For security, this file is **not** included in version control (added to `.gitignore`) and exists only on the server. It is shared securely with authorized team members only.

Additionally, ensure that the `DATA_DIR` folder has the necessary structure for storing client data, as outlined privately in the shared example.

---

## 1. Parameter Database Versioning Based on JSON Input

### Overview
With the new versioning feature, users can create, save, and manage multiple versions of the parameter database. Each JSON input file now records the specific parameter database version used at the time of its creation. This ensures that when the JSON input is loaded later, the associated parameter database version is automatically used, maintaining compatibility and preventing data discrepancies.

### Key Features
- **Parameter Database Management**:
  - Users can create a default parameter database.
  - Parameter databases can be edited, renamed, cloned, shared, and deleted, similar to JSON input files.
  - Management actions are straightforward and intuitive, requiring no additional explanation.
  
- **Association of JSON Inputs with Parameter Databases**:
  - When creating a new JSON input, it is automatically assigned the default parameter database.
  - The association between a JSON input and its parameter database is maintained through a variable stored within the JSON file.

- **Loading JSON Inputs**:
  - When a user loads a previously saved JSON input, the application automatically loads the associated parameter database version recorded.
  - If the associated parameter database is missing, the application automatically assigns the default parameter database.

- **Selecting Parameter Databases**:
  - Users have the flexibility to select a different parameter database for a JSON input if desired.
  - A dropdown menu (`selectInput`) allows users to choose from available parameter databases.
  - This selection is accessible in the same way as selecting JSON input files.

### Using the Parameter Database Versioning Feature

- **Creating a New Parameter Database**:
   - Navigate to the Parameters Database section.
   - Click on the "New Parameters Database" button to create a default parameter database.
   - The new parameter database can be renamed, cloned, or deleted as needed.

- **Creating a New JSON Input**:
   - Go to the JSON Input section.
   - Click on "New Scenario" to generate a new scenario description.
   - The new JSON input is automatically associated with the default parameter database.

- **Associating a Different Parameter Database**:
   - While editing a JSON input, locate the "Parameter Database Selection" dropdown menu.
   - Select the desired parameter database from the list.
   - The JSON input is now associated with the selected parameter database.

- **Loading an Existing JSON Input**:
   - In the JSON Input section, use the "Select Scenario" dropdown menu to select a saved JSON file.
   - The application automatically loads the associated parameter database version.
   - If the associated parameter database is missing, the default parameter database is assigned.

#### Important Notes
- **No Duplicate Names**: Parameter databases must have unique names. The system prevents the creation of parameter databases with duplicate names to avoid confusion and ensure proper association with JSON inputs.
- **Automatic Assignment of Default Parameter Database**: If a JSON input's associated parameter database is missing, the application seamlessly assigns the default parameter database. This ensures that users can continue working without interruption.

---

## 2. Running Multiple Scenarios Simultaneously

### Overview
To enhance efficiency and productivity, users can now run multiple scenarios at the same time. This feature is particularly useful when users need to process several scenarios without waiting for each one to finish individually.

### Key Features
- **Multiple Scenario Selection**:
  - Users can select multiple scenarios to run simultaneously.
  - A multi-select dropdown menu (`selectInput` with multiple choices enabled) is provided for scenario selection.
  
- **Progress Indicators**:
  - While scenarios are running, a progress indicator is displayed.
  - This visual feedback allows users to monitor the execution status.

### Using the Multiple Scenarios Feature

- **Selecting Scenarios to Run**:
   - Navigate to the Scenario Execution section.
   - In the "Run Scenario" dropdown menu, select one or more scenarios by clicking on them. Multiple selections are allowed.

- **Running the Scenarios**:
   - After selecting the desired scenarios, click on the "Run Scenario(s)" button.
   - The application will initiate the execution of all selected scenarios simultaneously.

- **Accessing Results**:
   - Upon completion, scenario results are stored in the user's `Scenarios` folder.
   - Each scenario's results are saved in a folder named after the scenario input JSON file used for that scenario.
   - Users can navigate to the Scenarios section to view, analyze, or manage the results.

#### Important Notes
- **Overwriting Existing Result Folders**: 
  - If a result folder with the same name already exists, it will be overwritten by the new scenario run.
  - **Caution**: Overwriting is irreversible and will permanently replace previous results.
  - To preserve existing results, consider renaming the existing Scenario Input before running the scenario.

---

## 3. Implementing an Authentication System and Data Access Management

### Overview
The authentication system and data access management ensure secure, personalized experiences for users. Each user has a dedicated workspace with access control, safeguarding their data and enabling efficient file management. This system supports individual users, while "Super Users" have enhanced privileges to manage broader system access.

### Key Features

- **Data Folder Architecture**  
  Each user has a personal data folder within the application, named after their email address. This folder includes four main subfolders:

  - **Study_objects**: Stores user-created study scenarios and input files.
    - **Add New Files**: Users can create new, empty scenario files.
    - **Rename Files**: Users can change file names, ensuring each name is unique.
    - **Clone Files**: Duplicates files with a ‘copy’ prefix, which can then be renamed.
    - **Delete Files**: Permanently removes files.

  - **Parameters_database**: Contains parameter databases for scenarios. Supports similar file management actions as `Study_objects`.

  - **Scenarios**: Holds results of scenario executions.
    - When a scenario runs, a new folder is created with the scenario input file name.
    - If a folder with the same name exists, it will be overwritten.
    - Users can view, download, and delete results.

  - **Comparisons**: Stores results from scenario comparisons.
    - Shares the same behavior as `Scenarios` for overwriting existing folders.
    - Users can view, download, and delete results.

- **General File Management Actions**  
  Applicable across all folders, the following file management actions are available:

  - **Adding New Files**: Allows creation of empty scenario files in `Study_objects` or default parameter databases in `Parameters_database`.
  - **Renaming Files**: Enforces unique names within the folder for organization.
  - **Cloning Files**: Duplicates files with a ‘copy’ prefix, useful for creating backups or   starting points for new files.
  - **Deleting Files**: Irreversible action; a confirmation prompt is provided.

- **User Onboarding**  
  When a new user logs in for the first time, the application automatically sets up their personalized workspace:

  1. **Onboarding Questionnaire**  
     A pop-up window prompts the user to answer optional questions (e.g., "Why are you planning to use the tool?").
     - Responses are saved in `onboarding.csv` for future reference.
     - **Archiving and Versioning**: When modified, the current `onboarding.csv` file is archived with the modification date. A new file is created for updates.
     - **Data Storage**: Onboarding files are securely stored in the "Onboarding" folder in the data directory, configured via `.Renviron`. Super users can download these files via the Shiny app.

  2. **Creation of Personal Data Folder**  
     A new directory is created within the Users data folder, named after the user’s email, and includes:
     - `Study_objects`
     - `Parameters_database`
     - `Scenarios`
     - `Comparisons`

     This automated onboarding ensures that each user has a dedicated space to manage their work efficiently from the outset.

- **Super User Logic**  
  Super users have enhanced privileges, allowing them to perform additional tasks within the application:

  - **Database Navigation**:  
    Access the "Browse Files" feature to navigate through all user data folders, viewing contents within other users’ `Study_objects`, `Parameters_database`, `Scenarios` and `Comparisons` folders.

  - **Cloning Files from Other Users**:  
    - **Clone `Study_objects`**: Select any study object file from another user’s folder and confirm to copy it into your own `Study_objects` folder.
    - **Clone `Parameters_database`, `Scenarios`, or `Comparisons`**: Select a specific folder to clone all its contents into your corresponding folder. Individual files within these folders cannot be cloned separately.

- **Assigning Super User Status**  
  Assigning a user as a super user is managed directly within the Auth0 platform using the role management feature:

  1. **Create the 'Super User' Role**  
     In the Auth0 dashboard, navigate to the User Management section under Roles and create a new role named ‘Super User’.

  2. **Assign the Role**  
     Users can be assigned the role in the Roles section or directly within the Users tab:
     - In the Roles section, enter the user’s email address and assign the Super User role.
     - Alternatively, go to the Users tab, locate the user, and assign the Super User role from their profile.

---

### Shared Folder
The Shared Folder is a communal space within the application that provides users with access to pre-defined templates and resources curated by the team. These are designed to help users get started quickly by providing exemplary scenarios and parameter databases.

#### Scenario Description Tab:
- **Selecting a Shared Scenario**:
  - Users can browse the available scenarios in the Shared Examples section.
  - When a user selects a scenario from the shared examples, a pop-up window appears.

- **Cloning to User's Folder**:
  - The pop-up prompts the user to confirm cloning the selected scenario into their personal `Study_objects` folder.

- **Associated Parameter Databases**:
  - If the selected scenario has a related parameters database, it will also be cloned into the user's `Parameters_database` folder.
  - This ensures that all necessary components for the scenario are available to the user.

- **Post-Cloning Actions**:
  - Once cloned, the scenario and any associated parameter databases become part of the user's personal data folders.
  - The user can then visualize, edit, rename, or clone these files as needed.

- **Post-Cloning Actions**:
  - Once cloned, the scenario and any associated parameter databases become part of the user's personal data folders.
  - The user can then visualize, edit, rename, or clone these files as needed.

#### Parameters Database Tab:
- **Selecting a Shared Parameters Database**:
  - Users can view the list of parameter databases available in the Shared Examples section.
  - Upon selecting a parameters database, a pop-up window appears.

- **Cloning to User's Folder**:
  - The pop-up prompts the user to confirm cloning the selected parameters database into their personal `Parameters_database` folder.

- **Post-Cloning Actions**:
  - After cloning, the parameters database is available in the user's personal folder. The user can visualize, edit, rename, or clone the parameters database.

- **Restrictions**:
  - Users cannot add, rename, or delete files within the Shared Examples. This ensures that the templates remain consistent and unaltered for all users.

### Shared Pool
The Shared Pool is a collaborative feature that allows users to share their own scenario descriptions and parameter databases with other users directly within the application. This eliminates the need for external file transfers and facilitates seamless collaboration.

- **Available File Types**:
  - Users can share:
    - Scenario Descriptions (`.json` files)
    - Parameters Databases (folders)

- **How to Share**:
  - **Share Button**: The scenario description and parameters database tabs have a dedicated "Share" button. Clicking the "Share" button initiates the sharing process.
  - **Confirmation**: A confirmation pop-up appears, asking the user to confirm sharing the selected file. Upon confirmation, the file is added to the Shared Pool with the same name.

- **Overwriting in the Shared Pool**:
  - **Name Conflicts**: If a file with the same name already exists in the Shared Pool, it will be overwritten by the new file. To avoid unintentional overwrites, consider using unique names or appending a version number.

- **Accessing Shared Files from the Pool**:
  - **Searching for Files**:
    - Users must type the full file name (for Scenario Descriptions, include the .json extension, e.g., `example_scenario.json`).
    - For Parameters Databases, users should type the exact folder name.

  - **Verification and Cloning**:
    - After typing the file name, users click the "OK" button to verify the file availability in the Shared Pool. If the file is found, a pop-up window asks the user to confirm cloning it into their personal folder.

  - **Post-Cloning Actions**:
    - After cloning, the file is available in the user's personal folder for use (e.g., visualization, editing, renaming, or deleting).

#### Benefits of the Shared Pool
- **Enhanced Collaboration**: Allows users to share their work with others easily, fostering a collaborative environment.
- **No Need for External Transfers**: Eliminates the necessity to download and upload files for sharing purposes.

#### Important Notes
- **Overwriting Shared Files**: Sharing a file with the same name as an existing one in the Shared Pool will overwrite it without additional warnings. Use unique file names to avoid overwriting others' files.
- **No Editing of Shared Pool Directly**: Users cannot edit or delete files directly within the Shared Pool. To modify a shared file, clone it to your personal folder, make changes, and then share it back to the pool if desired.

---
