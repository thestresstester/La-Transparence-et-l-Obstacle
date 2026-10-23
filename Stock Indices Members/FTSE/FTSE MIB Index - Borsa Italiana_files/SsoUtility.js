
// Provide a default path to dwr.engine
if (dwr == null) var dwr = {};
if (dwr.engine == null) dwr.engine = {};
if (DWREngine == null) var DWREngine = dwr.engine;

if (SsoUtility == null) var SsoUtility = {};
SsoUtility._path = '/borsa/dwr';
SsoUtility.getSsoUserDwr = function(callback) {
  dwr.engine._execute(SsoUtility._path, 'SsoUtility', 'getSsoUserDwr', callback);
}
