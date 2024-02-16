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
        private static bool IsItemInSequence(object item, IEnumerable<object> selectedItems) =>
            selectedItems.Contains(item);

        public object Convert(object[] values, Type targetType, object parameter, CultureInfo culture) =>
            values.Length >= 2
                ? (object)IsItemInSequence(values[0], (IEnumerable<object>)values[1])
                : null;

        public object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture) =>
            throw new NotImplementedException();
    }
}