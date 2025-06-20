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
	let num1 = 0
  let num2 = 1
  while(true){
    let res = num1 + num2
    yield num2
    num1 = num2
    num2 = res
  }
}

export function* Fib2() {
  const sqrt5 = Math.sqrt(5)
	const phi = (1 + sqrt5) / 2
  const psi = (1 - sqrt5) / 2
  let n = 1
  while(true){
    yield Math.floor(((phi ** n) - (psi ** n)) / sqrt5)
    n++
  }
}