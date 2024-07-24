using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Windows.Data;

namespace KoffeeUI {
    /// <summary>
    /// Given an item and sequence of selected items, returns whether the item is "selected".
    /// </summary>
    public class IsCustomSelectionConverter : IMultiValueConverter {
        public object Convert(object[] values, Type targetType, object parameter, CultureInfo culture) {
            if (values.Length >= 2) {
                var item = values[0];
                var selectedItems = values[1] as IEnumerable<object>;
                return selectedItems?.Contains(item);
            }
            return null;
        }

        public object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture) =>
            throw new NotImplementedException();
    }
}