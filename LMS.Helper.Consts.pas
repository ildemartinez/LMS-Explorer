unit LMS.Helper.Consts;

interface

const

  WSTOKEN = 'wstoken';

  // API
  WSFUNCTION = 'wsfunction';

  CORE_COURSE = 'core_course_';
  CORE_COURSE_GET_CATEGORIES = CORE_COURSE + 'get_categories';
  CORE_COURSE_GET_COURSES = CORE_COURSE + 'get_courses';

  CORE_ENROL = 'core_enrol_';
  CORE_ENROL_GET_ENROLLED_USERS = CORE_ENROL + 'get_enrolled_users';

  CORE_GROUP = 'core_group_';
  CORE_GROUP_GET_COURSE_GROUPS = CORE_GROUP + 'get_course_groups';

  CORE_USER = 'core_user_';
  CORE_USER_GET_USERS = CORE_USER + 'get_users';
  // URL

  COURSE_VIEW = '/course/view.php?id=%d';
  CATEGORY_VIEW = '/course/index.php?categoryid=%d';
  USERS_VIEW = '/user/index.php?id=%d&tifirst&tilast';
  USERS_VIEW_FIRSTNAME_LASTNAME = '/user/index.php?id=%d&tifirst=%s&tilast=%s';
  PROFILE_VIEW = '/user/profile.php?id=%d';
  PROFILE_VIEW_IN_COURSE = '/user/view.php?id=%d&course=%d';
  EDIT_PROFILE_IN_COURSE = '/user/editadvanced.php?id=%d&course=%d';
  EDIT_PROFILE = '/user/editadvanced.php?id=%d';
  EDIT_COURSE = '/course/edit.php?id=%d';

  USER_CREATE = '/user/editadvanced.php?id=-1';
  USERS_UPLOAD = '/admin/tool/uploaduser/index.php';

  // admin settings
  ADMIN_SETTINGS_EXTERNALSERVICES =
    '/admin/settings.php?section=externalservices';
  ADMIN_USER = '/admin/user.php';

implementation

end.
