export interface Route {}

export interface Routes {
  CoursesPage: Route;
  SchemasPage: Route;
  SettingsPage: Route;
  CloudProviderListPage: Route;
  NewCourse: Route;
  CourseEdit: (uuid?: string) => Route;
  SchemaEdit: (uuid?: string) => Route;
}
