package ParentesisPila;

import java.util.Stack;
public class Parentesis {

     // Método para verificar si los paréntesis están balanceados
     public static boolean isBalanced(String expression) {
        // Crear una pila para almacenar los paréntesis
        Stack<Character> stack = new Stack<>();

      
        return stack.isEmpty();
    }
    
    public static void main(String[] args) {
        // Expresiones de prueba
        String expression1 = "(()())"; 
        String expression2 = "((())"; 
        String expression3 = "())(";   

        // Evaluar si las expresiones están balanceadas
        System.out.println("Expression 1: " + isBalanced(expression1)); 
        System.out.println("Expression 2: " + isBalanced(expression2)); 
        System.out.println("Expression 3: " + isBalanced(expression3));
    }
}
