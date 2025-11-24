import { Routes } from '@angular/router';
import { LoginComponent } from './components/login/login.component';
import { MenuComponent } from './components/menu/menu.component';
import { AccountViewComponent } from './components/account-view/account-view.component';
import { AccountUpdateComponent } from './components/account-update/account-update.component';

/**
 * Application Routes
 */
export const routes: Routes = [
  { path: '', redirectTo: '/login', pathMatch: 'full' },
  { path: 'login', component: LoginComponent },
  { path: 'menu', component: MenuComponent },
  { path: 'accounts', component: AccountViewComponent },
  { path: 'accounts/:id/edit', component: AccountUpdateComponent },
  { path: '**', redirectTo: '/login' }
];
