
import {FormArray, FormControl, FormGroup} from "@angular/forms";

export class ChildTestModel {
  constructor(
    public commonId?:string,
    public commonVal?:string,
    public userArr?:string[]
  ){}
}

export class ArrayTestModel {
  constructor(
    public id?:number,
    public name?:string
  ){}
}

export class TestModel {
  constructor(
    public token?:string,
    public name?:string,
    public time?:number,
    public array?:ArrayTestModel[],
    public object?:{},
    public commonConfig?:ChildTestModel
  ){}

  private innerFunc(){
    console.log('i m private function')
  }

  get(){
    this.innerFunc()
    return this.token
  }
}

export class NDF {

  private static RELATIONS = {}
  private static $TYPE = '$type'
  private static $TYPE_OBJECT = '{}'
  private static $TYPE_ARRAY = '[]'
  private static $TYPE_STRING = 'string'
  private static $TYPE_NUMBER = 'number'
  private static $MODEL = '$model'
  private static $TO = '$to'

  private static type(relation:any):{type?:string,model?:any,primitive?:string}{
    let result:{type?:string,model?:any,primitive?:string} = {}
    if(relation[this.$TYPE]){
      if(relation[this.$TYPE] == this.$TYPE_ARRAY){
        result.type = this.$TYPE_ARRAY
        if(relation[this.$MODEL]) result.model = relation[this.$MODEL]
        else result.model = Object
      }else if(relation[this.$TYPE].endsWith(this.$TYPE_ARRAY)){
        result.type = this.$TYPE_ARRAY
        if(relation[this.$TYPE].replace(this.$TYPE_ARRAY,'')==this.$TYPE_OBJECT) result.model = Object
        else result.primitive = relation[this.$TYPE].replace(this.$TYPE_ARRAY,'')
      }else if(relation[this.$TYPE] == this.$TYPE_OBJECT){
        result.type = this.$TYPE_OBJECT
        if(relation[this.$MODEL]) result.model = relation[this.$MODEL]
        else result.model = Object
      }else{
        result.type = relation[this.$TYPE]
      }
    }else{
      result.type = this.$TYPE_OBJECT
      if(relation[this.$MODEL]) result.model = relation[this.$MODEL]
      else result.model = Object
    }
    return result
  }
  private static to(relation:any){
    if(!relation[this.$TO]) throw new Error(`not contain field:${this.$TO} (${JSON.stringify(relation)})`)
    return relation[this.$TO]
  }
  private static data(type:string,data:any){
    if(type.indexOf('string')>-1) return data?data.toString():data
    else if(type.indexOf('number')>-1) return data?+data:data
    else return data
  }
  private static typeof(data:any,type:string){
    return data && typeof data == type
  }

  

  static addRelation(name:string,relation:{}){
    if(this.RELATIONS[name]) throw new Error(`the relation has this name[${name}] is already exist`)
    this.RELATIONS[name] = relation
  }
  static getRelation(name:string){
    if(!this.RELATIONS[name]) throw new Error(`the relation has this name[${name}] is not exist`)
    return this.RELATIONS[name]
  }

  static n2d<T>(relation:any , data:any):T {
    let type = this.type(relation)
    let result = null
    if(type.type == this.$TYPE_ARRAY){
      result = []
      for(let item of data)
      {
        if(type.model){
          let obj = new type.model()
          for(let field in relation)
          {
            if(field.indexOf('$') == -1){
              obj[this.to(relation[field])] = this.n2d(relation[field],item[field])
            }
          }
          result.push(obj)
        }else{
          result.push(this.data(type.primitive,item))
        }
      }
    }else if(type.type == this.$TYPE_OBJECT){
      result = new type.model()
      for(let field in relation)
      {
        if(field.indexOf('$') == -1){
          result[this.to(relation[field])] = this.n2d(relation[field],data[field])
        }
      }
    }else{
      result = this.data(type.type,data)
    }
    return result
  }

  static d2n(relation:any, data:any){
    let type = this.type(relation)
    let result = null
    if(type.type == this.$TYPE_ARRAY){
      result = []
      for(let item of data)
      {
        if(type.model){
          let obj = {}
          for(let field in relation)
          {
            if(field.indexOf('$') == -1){
              obj[this.to(relation[field])] = this.d2n(relation[field],item[field])
            }
          }
          result.push(obj)
        }else{
          result.push(this.data(type.primitive,item))
        }
      }
    }else if(type.type == this.$TYPE_OBJECT){
      result = {}
      for(let field in relation)
      {
        if(field.indexOf('$') == -1){
          result[this.to(relation[field])] = this.d2n(relation[field],data[field])
        }
      }
    }else{
      result = this.data(type.type,data)
    }
    return result
  }

  static d2f(relation:any, data:any):FormGroup|FormArray|FormControl{
    let type = this.type(relation)
    let result = null
    if(type.type == this.$TYPE_ARRAY){
      result = new FormArray([])
      for(let item of data)
      {
        if(type.model){
          let obj = new FormGroup({})
          for(let field in relation)
          {
            if(field.indexOf('$') == -1){
              //obj[this.to(relation[field])] = this.n2d(relation[field],item[field])
              obj.setControl(this.to(relation[field]),this.d2f(relation[field],item[field]))
            }
          }
          result.push(obj)
        }else{
          result.push(new FormControl(this.data(type.primitive,item)))
        }
      }
    }else if(type.type == this.$TYPE_OBJECT){
      result = new FormGroup({})
      for(let field in relation)
      {
        if(field.indexOf('$') == -1){
          //result[this.to(relation[field])] = this.n2d(relation[field],data[field])
          result.setControl(this.to(relation[field]),this.d2f(relation[field],data[field]))
        }
      }
    }else{
      result = new FormControl(this.data(type.type,data))
    }
    return result
  }

  static f2d<T>(relation:any, data:FormGroup|FormArray|FormControl):T{
    let type = this.type(relation)
    let result = null
    if(type.type == this.$TYPE_ARRAY){
      result = []
      for(let item of data['controls'])
      {
        if(type.model){
          let obj = new type.model()
          for(let field in relation)
          {
            if(field.indexOf('$') == -1){
              obj[this.to(relation[field])] = this.f2d(relation[field],item['controls'][field])
            }
          }
          result.push(obj)
        }else{
          result.push(this.data(type.primitive,item.value))
        }
      }
    }else if(type.type == this.$TYPE_OBJECT){
      result = new type.model()
      for(let field in relation)
      {
        if(field.indexOf('$') == -1){
          result[this.to(relation[field])] = this.f2d(relation[field],data['controls'][field])
        }
      }
    }else{
      result = this.data(type.type,data.value)
    }
    return result
  }

  static n2f(relation:any, data:any):FormGroup|FormArray|FormControl{
    let type = this.type(relation)
    let result = null
    if(type.type == this.$TYPE_ARRAY){
      result = new FormArray([])
      for(let item of data)
      {
        if(type.model){
          let obj = new FormGroup({})
          for(let field in relation)
          {
            if(field.indexOf('$') == -1){
              obj.setControl(this.to(relation[field]),this.n2f(relation[field],item[field]))
            }
          }
          result.push(obj)
        }else{
          result.push(new FormControl(this.data(type.primitive,item)))
        }
      }
    }else if(type.type == this.$TYPE_OBJECT){
      result = new FormGroup({})
      for(let field in relation)
      {
        if(field.indexOf('$') == -1){
          result.setControl(this.to(relation[field]),this.n2f(relation[field],data[field]))
        }
      }
    }else{
      result = new FormControl(this.data(type.type,data))
    }
    return result
  }

  static f2n(relation:any, data:any){
    let type = this.type(relation)
    let result = null
    if(type.type == this.$TYPE_ARRAY){
      result = []
      for(let item of data['controls'])
      {
        if(type.model){
          let obj = new type.model()
          for(let field in relation)
          {
            if(field.indexOf('$') == -1){
              obj[this.to(relation[field])] = this.f2d(relation[field],item['controls'][field])
            }
          }
          result.push(obj)
        }else{
          result.push(this.data(type.primitive,item.value))
        }
      }
    }else if(type.type == this.$TYPE_OBJECT){
      result = new type.model()
      for(let field in relation)
      {
        if(field.indexOf('$') == -1){
          result[this.to(relation[field])] = this.f2d(relation[field],data['controls'][field])
        }
      }
    }else{
      result = this.data(type.type,data.value)
    }
    return result
  }

  static test(){
    var relation = {
      $type:'{}',
      $model:TestModel,
      token:{
        $to:'token',
        $type:'string'
      },
      name:{
        $to:'name',
        $type:'string'
      },
      time:{
        $to:'time',
        $type:'number'
      },
      array:{
        $to:'array',
        $type:'{}[]',
        $model:ArrayTestModel,
        id:{
          $to:'id',
          $type:'number'
        },
        name:{
          $to:'name',
          $type:'string'
        }
      },
      object:{
        $to:'object',
        $type:'{}',
        oid:{
          $to:'oid',
          $type:'string'
        },
        secret:{
          $to:'secret',
          $type:'string'
        }
      },
      commonConfig:{
        $to:'commonConfig',
        $type:'{}',
        $model:ChildTestModel,
        commonId:{
          $to:'commonId',
          $type:'string'
        },
        commonVal:{
          $to:'commonVal',
          $type:'string'
        },
        userArr:{
          $to:'userArr',
          $type:'string[]'
        }
      }
    }
    let data = {
      token:null,
      name:'lfbhlx',
      time:123456,
      array:[{id:1,name:'apple'},{id:2,name:'banana'}],
      object:{
        oid:'myoid',
        secret:'mysecret'
      },
      commonConfig:{
        commonId:'123d1231',
        commonVal:'123d1231d',
        userArr:['one','two','three']
      }
    }
    this.addRelation('a',relation)

    let dm = this.n2d<TestModel>(this.getRelation('a'),data)
    dm.get()
    console.log(dm)
    let nm = this.d2n(this.getRelation('a'),dm)
    console.log(nm)
    let fm = this.d2f(this.getRelation('a'),dm)
    console.log(fm)
    let dm2 = this.f2d<TestModel>(this.getRelation('a'),fm)
    console.log(dm2)
    let fm2 = this.n2f(this.getRelation('a'),nm)
    console.log(fm2)
    let nm2 = this.f2n(this.getRelation('a'),fm2)
    console.log(nm2)
  }

}
