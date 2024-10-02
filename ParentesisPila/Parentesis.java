package ParentesisPila;

import java.util.Stack;
public class Parentesis {

     // Método para verificar si los paréntesis están balanceados
    public static boolean isBalanced(String expression, boolean showProgress) {
        Stack<Character> stack = new Stack<>();

        // Iterar sobre cada carácter de la expresión
        for (char ch : expression.toCharArray()) {
            try {
                // Mostrar el carácter que se está procesando si showProgress es true
                if (showProgress) {
                    System.out.println("Procesando carácter: " + ch);
                }
                
                if (ch == '(') {
                    stack.push(ch);
                    if (showProgress) {
                        System.out.println("Paréntesis de apertura encontrado.");
                    }
                } else if (ch == ')') {
                    if (stack.isEmpty()) {
                        if (showProgress) {
                            System.out.println("Paréntesis de cierre encontrado pero la pila está vacía. No está balanceado.");
                        }
                        return false;
                    }
                    stack.pop();
                    if (showProgress) {
                        System.out.println("Paréntesis de cierre encontrado.");
                    }
                }

                // Mostrar el estado actual de la pila si showProgress es true
                if (showProgress) {
                    printStack(stack);
                    // Introduce un retraso de 500 milisegundos (0.5 segundos)
                    Thread.sleep(500);
                }

            } catch (InterruptedException e) {
                System.out.println("Se produjo un error en el delay: " + e.getMessage());
            }
        }

        // Retorna true si la pila está vacía (es decir, los paréntesis están balanceados)
        return stack.isEmpty();
    }

      // Método para imprimir el estado de la pila
      private static void printStack(Stack<Character> stack) {
        System.out.println("Estado actual de la pila:");
        if (stack.isEmpty()) {
            System.out.println("(vacía)");
        } else {
            for (int i = stack.size() - 1; i >= 0; i--) {
                System.out.println("| " + stack.get(i) + " |");
            }
        }
        System.out.println("-----");  // Indicador de la base de la pila
    }
    
    public static void main(String[] args) {
        // Expresiones de prueba
        String expression1 = "(()()()((((((()))))))(())()())"; 
        String expression2 = "((())"; 
        String expression3 = "())(";   

        // Evaluar si las expresiones están balanceadas
        boolean showProgress = true;  // Cambia esto a false para no mostrar el progreso

        System.out.println("Evaluando Expression 1: ");
        System.out.println("Resultado: " + isBalanced(expression1, showProgress) + "\n");

        // System.out.println("Evaluando Expression 2: ");
        // System.out.println("Resultado: " + isBalanced(expression2, showProgress) + "\n");

        // System.out.println("Evaluando Expression 3: ");
        // System.out.println("Resultado: " + isBalanced(expression3, showProgress) + "\n");
    }
}
