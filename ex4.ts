//Q1
export function all<T>(promises : Array<Promise<T>>) : Promise<Array<T>> {

  return new Promise<T[]>((resolve, reject) => {
    let res: T[] = new Array(promises.length)
    let counter = 0
    if(promises.length === 0){
      resolve([])
    }

    for(let i = 0; i < promises.length; i++){
      promises[i].then((value: T) => {
        res[i] = value
        counter++
        if(counter === promises.length){
          resolve(res)
        }
      }).catch((err => reject(err)))
    }
  })
}

  
// Q2
export function* Fib1() {
	// @TODO
  return undefined;
}


export function* Fib2() {
	// @TODO
  return undefined;
}
