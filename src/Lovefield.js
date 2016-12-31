exports.dynGet = function dynGet(str){
  return function(obj){
    return obj[str];
  };
};

exports.showHCons = function showHCons(obj){
  return JSON.stringify(obj);
};

exports.hconsImpl = function(label){
  return function(v){
    return function(obj){
      return obj[label] = v;
    };
  };
};
exports.getLabels = function(obj){
  return Object.keys(obj);
};
exports.getSchema = function(db){
  return db.getSchema();
};
exports.getTable = function(schema){
  return function(name){
    return schema.table(name);
  };
};
exports.insertImpl = function(db){
  return function(table){
    return function(values){
      return function(){
        var row = table.createRow(values);
        return db.insertOrReplace().into(table).values([row]).exec();
      };
    };
  };
};

exports.buildSchema = function(name){
  return function(version){
    return function(){
      return lf.schema.create(name, version);
    };
  };
};
exports.createImpl = function(schemaBuilder){
  return function(name){
    return function(cols){
      return function(pk){
        return function(){
          var tableBuilder = schemaBuilder.createTable(name);
          for (var colName in cols){
            tableBuilder.addColumn(colName, cols[colName]);
          }
          tableBuilder.addPrimaryKey(pk);
        };
      };
    };
  };
};
exports.connectImpl = function(schemaBuilder){
  return function(){
    return schemaBuilder.connect();
  };
};
exports.runQueryImpl = function(db){
  return function(q){
    return function(){
      // projections
      var tableI, colI, table, colName, tableEntry, schema = db.getSchema(), cols = [];
      for (tableI = 0; tableI < q.projections.length; tableI++){
        tableEntry = q.projections[tableI];
        table = schema.table(tableEntry.tn);
        for (colI = 0; colI < tableEntry.cns.length; colI++){
          colName = tableEntry.cns[colI];
          cols.push(table[colName]);
        }
      }
      var q1 = db.select.apply(db, cols);
      // we are not using implicit inner joins -> only pass first table in .from
      // We do however need to pass in all the fields of the explicitely joined tables
      var q2 = q1.from(schema.table(q.projections[0].tn));

      // joins
      var joinI, join, q3 = q2, t1, t2, c1, c2;
      for(joinI = 0; joinI < q.joins.length; joinI++){
        join = q.joins[joinI];
        t1 = schema.table(join.tn1);
        t2 = schema.table(join.tn2);
        c1 = t1[join.cn1];
        c2 = t2[join.cn2];
        q3 = q3.innerJoin(t2, c2.eq(c1));
      }

      // order of tables in from and join is important!
      // from A, then join B.col on A.col!

      // predicates
      var predicateI, pred, preds = [], q4 = q3;
      if (q.predicates.length === 1){
        pred = q.predicates[0];
        table = schema.table(pred.tn);
        colName = table[pred.cn];
        q4 = q4.where(colName[pred.op](pred.v));
      } else if (q.predicates.length > 1){
        for (predicateI = 0; predicateI < q.predicates.length; predicateI++){
          pred = q.predicates[predicateI];
          table = schema.table(pred.tn);
          colName = table[pred.cn];
          preds.push(colName[pred.op](pred.v));
        }
        q4 = q4.where(lf.op.and.apply(lf.op, preds));
      }

      return q4.exec().then(function(res){
        if (q.projections.length === 1){
          // Wrapping into object with 1 key = tableName for consistency
          var r = [], l;
          for (var i = 0; i < res.length; i++){
            l = {};
            l[q.projections[0].tn] = res[i];
            r.push(l);
          }
          return r;
        } else {
          return res;
        }
      });
    };
  };
};
exports.deleteDatabase = function(str){
  return function(){
    indexedDB.deleteDatabase(str);
  };
};
