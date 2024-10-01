public class Cambio {
  static int[] monedas = {1, 2, 10, 20, 50, 100};

  public static int contarCombinaciones(int cantidad, int indice) {
      if (cantidad == 0) {
          return 1;
      }

      if (cantidad < 0 || indice >= monedas.length) {
          return 0;
      }

      // Caso recursivo:
      // Opción 1: usar la moneda en el índice actual y seguir buscando con la misma moneda
      int incluir = contarCombinaciones(cantidad - monedas[indice], indice);

      // Opción 2: no usar la moneda actual, probar con la siguiente moneda
      int excluir = contarCombinaciones(cantidad, indice + 1);

      // El número total de combinaciones es la suma de las dos opciones
      return incluir + excluir;
  }

  public static void main(String[] args) {
      int cantidad = 200;
      int combinaciones = contarCombinaciones(cantidad, 0);
      System.out.println("Número de combinaciones para " + cantidad + ": " + combinaciones);
  }
}
