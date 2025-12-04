# CardDemo POC - Quick Start Guide

## 🚀 Running the Complete Application

### Option 1: Start Everything (Recommended)

```bash
cd src/poc/carddemo-poc
./start-all.sh
```

This will:
- Start the backend on http://localhost:8080
- Start the frontend on http://localhost:4200
- Open in separate terminal windows (macOS)

### Option 2: Start Separately

**Terminal 1 - Backend:**
```bash
cd src/poc/carddemo-poc
./start-poc.sh
```

**Terminal 2 - Frontend:**
```bash
cd src/poc/carddemo-poc
./start-frontend.sh
```

## 🎯 Testing the Application

### 1. Open Browser
Navigate to: **http://localhost:4200**

### 2. Login
- **User ID**: `ADMIN01`
- **Password**: `ADMIN01`
- Click **"ENTER - Sign On"**

### 3. Main Menu
After successful login, you'll see:
- Your user information (name, role)
- Menu options (filtered by role)
- Available functions

### 4. Test Admin vs Regular User

**Admin User (sees all options):**
- User ID: `ADMIN01`
- Password: `ADMIN01`
- Has access to "User Administration"

**Regular User (limited options):**
- User ID: `USER01`
- Password: `USER01`
- Cannot access admin functions

### 5. Logout
- Click **"F3 - Logout"** button
- Returns to login screen
- Session cleared

## ✅ What Should Work

- ✅ Login with valid credentials
- ✅ Login rejection with invalid credentials
- ✅ Case-insensitive credential matching (admin01 = ADMIN01)
- ✅ Role-based menu filtering
- ✅ Logout and session management
- ✅ Error messages for missing/invalid fields
- ✅ Loading state during authentication

## 🧪 Testing Scenarios

### Test Case 1: Successful Admin Login
```
1. Enter: ADMIN01 / ADMIN01
2. Click "ENTER - Sign On"
3. ✅ Should navigate to main menu
4. ✅ Should show "Administrator" role
5. ✅ Should see 6 menu options including "User Administration"
```

### Test Case 2: Invalid Password
```
1. Enter: ADMIN01 / WRONGPASS
2. Click "ENTER - Sign On"
3. ✅ Should show error: "Invalid user ID or password"
4. ✅ Should stay on login screen
5. ✅ Password field should be cleared
```

### Test Case 3: Non-Existent User
```
1. Enter: NOUSER / PASSWORD
2. Click "ENTER - Sign On"
3. ✅ Should show error: "Invalid user ID or password"
```

### Test Case 4: Missing Fields
```
1. Leave User ID empty
2. Enter password
3. ✅ "ENTER" button should be disabled
4. ✅ Cannot submit form
```

### Test Case 5: Case Insensitivity
```
1. Enter: admin01 / admin01 (lowercase)
2. Click "ENTER - Sign On"
3. ✅ Should login successfully (converted to uppercase)
```

### Test Case 6: Regular User Access
```
1. Enter: USER01 / USER01
2. Click "ENTER - Sign On"
3. ✅ Should navigate to main menu
4. ✅ Should show "Regular User" role
5. ✅ Should see 5 menu options (no User Administration)
```

### Test Case 7: Logout
```
1. Login as any user
2. Navigate to main menu
3. Click "F3 - Logout"
4. ✅ Should return to login screen
5. ✅ Session should be cleared
6. ✅ Manual navigation to /menu should not work (redirect to login)
```

## 🔍 Debugging

### Backend Not Starting?
```bash
# Check if port 8080 is available
lsof -i :8080

# Check backend logs
cd src/poc/carddemo-poc
./mvnw spring-boot:run
# Look for errors in console
```

### Frontend Not Starting?
```bash
# Check if port 4200 is available
lsof -i :4200

# Check Node/npm installation
node --version    # Should be 18+
npm --version

# Reinstall dependencies
cd src/poc/carddemo-poc/frontend
rm -rf node_modules package-lock.json
npm install
```

### CORS Errors in Browser Console?
- Make sure backend is running on http://localhost:8080
- Check backend console for CORS configuration
- Frontend must be on http://localhost:4200

### Can't Connect to Backend?
```bash
# Test backend directly
curl http://localhost:8080/api/auth/health

# Should return: "Authentication service is up and running"
```

## 📊 Available Test Users

| User ID | Password | Role | Description |
|---------|----------|------|-------------|
| ADMIN01 | ADMIN01  | Admin | System Administrator - Full access |
| USER01  | USER01   | User | Regular user - Standard access |
| USER02  | USER02   | User | Regular user - Standard access |
| USER03  | USER03   | User | Regular user - Standard access |
| USER04  | USER04   | User | Regular user - Standard access |

## 🛠️ Useful Development URLs

- **Frontend**: http://localhost:4200
- **Backend API**: http://localhost:8080/api
- **Health Check**: http://localhost:8080/api/auth/health
- **H2 Console**: http://localhost:8080/h2-console
  - JDBC URL: `jdbc:h2:file:./data/carddemo`
  - Username: `sa`
  - Password: (empty)

## 📱 Browser DevTools Tips

### View Network Requests
1. Open DevTools (F12)
2. Go to Network tab
3. Login
4. See POST to `/api/auth/login`
5. Check request/response payloads

### View Session Storage
1. Open DevTools (F12)
2. Go to Application tab
3. Expand "Session Storage"
4. See `currentUser` after login
5. Should be cleared after logout

### View Console Logs
1. Open DevTools (F12)
2. Go to Console tab
3. See authentication flow logs
4. Check for errors

## 🎬 Demo Script

**For stakeholder demonstrations:**

1. **Start Application**
   ```bash
   cd src/poc/carddemo-poc
   ./start-all.sh
   ```
   Wait for both services to start (~30 seconds)

2. **Show Login Screen**
   - Point out CICS-style terminal theme
   - Explain credential fields
   - Show function key bar

3. **Demo Invalid Login**
   - Enter: ADMIN01 / WRONGPASS
   - Show error message
   - Explain security (no user enumeration)

4. **Demo Successful Admin Login**
   - Enter: ADMIN01 / ADMIN01
   - Show navigation to menu
   - Point out user information display
   - Show 6 menu options

5. **Demo Regular User**
   - Logout
   - Login as: USER01 / USER01
   - Show 5 menu options (no admin)
   - Explain role-based filtering

6. **Show Backend API** (optional technical demo)
   - Open http://localhost:8080/h2-console
   - Show database tables (USERS)
   - Show sample data

7. **Explain Next Steps**
   - This POC validates authentication logic
   - Ready for account/card management modules
   - Production version will add security (JWT, bcrypt)

## 🎯 Success Criteria

**The POC is successful if:**
- ✅ Users can login with valid credentials
- ✅ Invalid credentials are rejected
- ✅ Admin users see all menu options
- ✅ Regular users see filtered menu
- ✅ Logout clears session
- ✅ UI matches CICS terminal aesthetic
- ✅ Backend API responds correctly
- ✅ All test scenarios pass

## 🔄 Stopping the Application

### If Started with start-all.sh (macOS)
- Close the Terminal windows
- Or press Ctrl+C in each window

### If Started Separately
- Press Ctrl+C in backend terminal
- Press Ctrl+C in frontend terminal

### Force Stop All
```bash
# Kill backend
lsof -ti:8080 | xargs kill -9

# Kill frontend dev server
lsof -ti:4200 | xargs kill -9
```

## 📝 Notes

- Backend uses H2 file database (data persists between restarts)
- Frontend uses session storage (clears on browser close)
- Both services must be running for full functionality
- Use Chrome/Firefox/Safari (avoid IE)
- Clear browser cache if seeing stale data

---

**Questions?** Check the README files:
- Backend: `src/poc/carddemo-poc/README.md`
- Frontend: `src/poc/carddemo-poc/frontend/README.md`
- Summary: `src/poc/carddemo-poc/POC-IMPLEMENTATION-SUMMARY.md`
