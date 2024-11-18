using System;
using System.Globalization;
using System.Linq;
using System.Windows.Data;
using System.Windows.Markup;

namespace KoffeeUI {
    /// <summary>
    /// Maps boolean to one of two integer parameters delimited by pipe '|'. True maps to the first value, false to the
    /// second value.
    /// </summary>
    public class BoolToIntParamConverter : MarkupExtension, IValueConverter {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture) {
            var parameters = ParseParams(parameter);
            return (bool)value ? parameters[0] : parameters[1];
        }

        private static int[] ParseParams(object parameter) {
            try {
                var parameters = ((string)parameter).Split('|').Select(s => System.Convert.ToInt32(s)).ToArray();
                if (parameters.Length != 2) {
                    throw new ArgumentException("Expected 2 arguments but got " + parameters.Length);
                }
                return parameters;
            } catch (Exception ex) {
                throw new ArgumentException("ConverterParameter must be two integers delimited by '|'", ex);
            }
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture) {
            throw new NotImplementedException();
        }

        public override object ProvideValue(IServiceProvider serviceProvider) => this;
    }
}