# Manual Startup Guide

If the automatic scripts aren't working, follow these manual steps:

## Step 1: Start Backend

Open a **new terminal window** and run:

```bash
cd /Users/koen.goovaerts/source/repo/aws-mainframe-modernization-carddemo/src/poc/carddemo-poc

./mvnw spring-boot:run
```

Wait for the message:
```
Started CardDemoPocApplication in X.XXX seconds
```

The backend should now be running on **http://localhost:8080**

## Step 2: Start Frontend

Open **another new terminal window** and run:

```bash
cd /Users/koen.goovaerts/source/repo/aws-mainframe-modernization-carddemo/src/poc/carddemo-poc/frontend

# Install dependencies (first time only)
npm install

# Start dev server
npm start
```

Wait for the message:
```
** Angular Live Development Server is listening on localhost:4200 **
```

The frontend should now be running on **http://localhost:4200**

## Step 3: Test the Application

1. Open your web browser
2. Navigate to: **http://localhost:4200**
3. You should see the login screen
4. Enter credentials:
   - User ID: `ADMIN01`
   - Password: `ADMIN01`
5. Click "ENTER - Sign On"
6. You should see the main menu

## Troubleshooting

### Backend Won't Start

**Check if port 8080 is already in use:**
```bash
lsof -i :8080
```

If something is using it, kill it:
```bash
kill -9 <PID>
```

**Check Java version:**
```bash
java --version
```
Must be Java 21 or higher.

### Frontend Won't Start

**Check if port 4200 is already in use:**
```bash
lsof -i :4200
```

If something is using it, kill it:
```bash
kill -9 <PID>
```

**Delete node_modules and reinstall:**
```bash
cd frontend
rm -rf node_modules package-lock.json
npm install
```

### CORS Errors

Make sure:
1. Backend is running on http://localhost:8080
2. Frontend is running on http://localhost:4200
3. Both must be exactly these URLs for CORS to work

### Can't Connect to Backend

Test the backend directly:
```bash
curl http://localhost:8080/api/auth/health
```

Should return: `"Authentication service is up and running"`

## Alternative: Use Individual Commands

### Backend Only
```bash
cd /Users/koen.goovaerts/source/repo/aws-mainframe-modernization-carddemo/src/poc/carddemo-poc
./mvnw clean spring-boot:run
```

### Frontend Only  
```bash
cd /Users/koen.goovaerts/source/repo/aws-mainframe-modernization-carddemo/src/poc/carddemo-poc/frontend
npx ng serve
```

##  Success Indicators

**Backend is ready when you see:**
```
Started CardDemoPocApplication
Tomcat started on port 8080
```

**Frontend is ready when you see:**
```
✔ Browser application bundle generation complete
** Angular Live Development Server is listening on localhost:4200 **
```

## Stopping the Services

In each terminal window, press: **Ctrl+C**

---

**Still having issues?** Check the full logs in each terminal window for specific error messages.
