(function () {
  rcversion = '1.0';
  load('underscore.js');

  (function () {
    var cmdCount = 0;
    var isMaster = db.isMaster();

    if (isMaster.setName) {
      prompt = function () {
        return isMaster.setName +  '(' + (isMaster.ismaster ? 'pri' : 'sec' ) + '):' + db + '-' + (cmdCount++) + ' > ';
      };
    } else {
      prompt = function () {
        return db + '-' + (cmdCount++) + ' > ';
      };
    }
  })();

  (function () {
    _.mixin({
      // slow query
      sloGroupBy: function (field) {
        return db.system.profile.aggregate({$group: {'_id': '$' + field, 'total': {$sum: 1}}});
      },
      sloByCost: function (query, limit) {
        if (!limit) { limit = 5; }
        return db.system.profile.find(query).sort({millis: -1}).limit(limit);
      },
      sloRecent: function (query, limit) {
        if (!limit) { limit = 5; }
        return db.system.profile.find(query).sort({$natural: -1}).limit(limit);
      },

      // get the current timestamp
      now: function () {
        return (new Date()).getTime() / 1000;
      },

      // Create an object id based on a timestamp
      oid: function(timestamp) {
        if (!timestamp) {
          return new ObjectId();
        }

        // Convert string date to Date object (otherwise assume timestamp is a date)
        if (!(timestamp instanceof Date)) {
          timestamp = new Date(timestamp);
        }

        // Convert date object to hex seconds since Unix epoch
        var hexSeconds = Math.floor(timestamp / 1000).toString(16);

        // Create an ObjectId with that hex timestamp
        return ObjectId(hexSeconds + "0000000000000000");
      },
    });
  })();
})();
