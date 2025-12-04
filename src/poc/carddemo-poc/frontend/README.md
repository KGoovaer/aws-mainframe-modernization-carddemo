# CardDemo POC - Angular Frontend

This is the Angular 18 frontend for the CardDemo POC application.

## Features

- **User Authentication**: Login screen with user ID and password validation
- **Main Menu**: Role-based menu system with admin/regular user options
- **Mainframe Terminal Theme**: CSS styling that mimics CICS terminal screens
- **Responsive Design**: Works on desktop and tablet devices

## Architecture

- **Angular 18** with standalone components
- **Reactive Forms** for form handling
- **RxJS** for reactive data management
- **HttpClient** for REST API communication
- **Session Storage** for client-side session management

## Project Structure

```
frontend/
├── src/
│   ├── app/
│   │   ├── components/
│   │   │   ├── login/           # Login screen (COSGN00C)
│   │   │   └── menu/            # Main menu screen (COMEN01C)
│   │   ├── services/
│   │   │   └── auth.service.ts  # Authentication service
│   │   ├── models/
│   │   │   └── auth.model.ts    # Data models
│   │   ├── app.component.ts     # Root component
│   │   ├── app.config.ts        # App configuration
│   │   └── app.routes.ts        # Route definitions
│   ├── styles.css               # Global styles (terminal theme)
│   ├── index.html               # HTML entry point
│   └── main.ts                  # Bootstrap file
├── package.json                 # Dependencies
├── angular.json                 # Angular CLI config
└── tsconfig.json                # TypeScript config
```

## Prerequisites

- **Node.js** 18.x or higher
- **npm** 9.x or higher
- **Backend API** running on http://localhost:8080

## Setup

1. **Install dependencies**:
   ```bash
   cd frontend
   npm install
   ```

2. **Start development server**:
   ```bash
   npm start
   # or
   ng serve
   ```

3. **Access the application**:
   - Open browser to http://localhost:4200
   - Default credentials: `ADMIN01` / `ADMIN01`

## Development

### Run Development Server
```bash
npm start
```
The application will automatically reload when you make changes.

### Build for Production
```bash
npm run build
```
Production build artifacts are stored in `dist/`.

### Run Tests
```bash
npm test
```

## Components

### Login Component (`login.component.ts`)
- Maps to COBOL program **COSGN00C**
- Implements **US-001**: Successful Login with Valid Credentials
- Features:
  - User ID and password input fields
  - Case-insensitive credential handling
  - Form validation
  - Error message display
  - Loading state during authentication

### Menu Component (`menu.component.ts`)
- Maps to COBOL program **COMEN01C**
- Implements **US-008**: Role-Based Login Routing
- Features:
  - User information display
  - Role-based menu filtering
  - Menu item selection
  - Logout functionality

### Auth Service (`auth.service.ts`)
- Handles authentication API calls
- Manages user session state
- Provides observables for reactive updates
- Session storage integration

## API Integration

The frontend communicates with the Spring Boot backend:

- **Base URL**: http://localhost:8080/api
- **Login**: POST `/auth/login`
- **Logout**: POST `/auth/logout`

CORS is configured on the backend to allow requests from `http://localhost:4200`.

## Styling

The application uses a **mainframe terminal theme** with:
- Dark blue/black background (#0a0e27)
- Green terminal text (#00ff00)
- Monospace font (Courier New)
- Border glow effects
- CICS-style screen headers
- Function key bar at bottom

## Default Test Users

| User ID | Password | Role | Access |
|---------|----------|------|--------|
| ADMIN01 | ADMIN01  | Admin | All menu options including User Administration |
| USER01  | USER01   | User | Standard menu options only |

## User Stories Implemented

- **US-001**: Successful Login with Valid Credentials
- **US-002**: Login Failure with Invalid Password  
- **US-003**: Login Failure with Non-Existent Username
- **US-005**: User Logout
- **US-008**: Role-Based Login Routing
- **US-011**: Missing Required Fields Validation

## Business Rules

- **Rule 001**: Credential Case Insensitivity - All credentials converted to uppercase
- **Rule 002**: Required Field Validation - User ID and password required
- **Rule 003**: Role-Based Menu Filtering - Admin users see additional options

## Known Limitations (POC)

- No route guards (authentication check on protected routes)
- No JWT token-based authentication
- Session storage only (clears on browser close)
- Other menu options not yet implemented
- No form field length validation beyond maxlength attribute
- Basic error handling only

## Next Steps

After POC validation, the production version will add:
- Route guards for authentication protection
- JWT token-based authentication
- Refresh token handling
- More comprehensive error handling
- Additional screens (accounts, cards, transactions)
- E2E testing with Cypress
- Accessibility improvements
- Production-ready build optimization
