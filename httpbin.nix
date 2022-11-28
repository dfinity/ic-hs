{ lib
, brotlipy
, buildPythonPackage
, decorator
, fetchpatch
, fetchFromGitHub
, flask
, flask-limiter
, itsdangerous
, markupsafe
, raven
, six
, pytestCheckHook
}:

buildPythonPackage rec {
  pname = "httpbin";
  version = "0.7.0";
  format = "setuptools";

  src = fetchFromGitHub {
    name = "httpbin";
    owner = "mraszyk";
    repo = "httpbin";
    rev = "c6f2830f366635ed0afef1e6f5d5c48428950d36";
    sha256 = "sha256-S4dAuV925YM+KLA0u/eoZkICWl5ExvfKD2+34rbpm7Q=";
  };

  patches = [
    (fetchpatch {
      # Replaces BaseResponse class with Response class for Werkezug 2.1.0 compatibility
      # https://github.com/postmanlabs/httpbin/pull/674
      url = "https://github.com/postmanlabs/httpbin/commit/5cc81ce87a3c447a127e4a1a707faf9f3b1c9b6b.patch";
      hash = "sha256-SbEWjiqayMFYrbgAPZtSsXqSyCDUz3z127XgcKOcrkE=";
    })
  ];

  propagatedBuildInputs = [
    brotlipy
    flask
    flask-limiter
    markupsafe
    decorator
    itsdangerous
    raven
    six
  ];

  checkInputs = [
    pytestCheckHook
  ];

  pytestFlagsArray = [
    "test_httpbin.py"
  ];

  disabledTests = [
    # Tests seems to be outdated
    "test_anything"
    "test_get"
    "test_redirect_n_equals_to_1"
    "test_redirect_n_higher_than_1"
    "test_redirect_to_post"
    "test_relative_redirect_n_equals_to_1"
    "test_relative_redirect_n_higher_than_1"
  ];

  pythonImportsCheck = [
    "httpbin"
  ];

  meta = with lib; {
    description = "HTTP Request and Response Service";
    homepage = "https://github.com/kennethreitz/httpbin";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
