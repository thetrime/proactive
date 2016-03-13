package org.proactive.ui;

import javax.swing.JTable;
import javax.swing.JScrollPane;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;
import java.util.List;
import java.util.LinkedList;
import java.util.HashMap;
import java.awt.Component;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ReactComponent;

public class Table extends ReactComponent 
{
   JTable table;
   JScrollPane scrollPane;
   ReactTableModel model = new ReactTableModel();
   TableCellRenderer cellRenderer = new ReactTableCellRenderer();
   public Table()
   {
      cellRenderer = new ReactTableCellRenderer();
      table = new JTable(model)
         {
            public TableCellRenderer getCellRenderer(int row, int column)
            {
               return cellRenderer;
            }
         };
      scrollPane = new JScrollPane(table);
   }
   
   public class ReactTableCellRenderer implements TableCellRenderer
   {      
      public ReactTableCellRenderer()
      {
      }
      
      public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column)
      {
         return ((ReactComponent)value).getAWTComponent();
      }
   }

   public class ReactTableModel extends AbstractTableModel
   {
      protected List<Row> rows = new LinkedList<Row>();
      public void addRow(Row row)
      {
         rows.add(row);
      }

      public void insertRowAt(Row row, int index)
      {
         rows.add(index, row);
      }

      public void removeRow(Row row)
      {
         rows.remove(row);
      }

      public void removeRowAt(int index)
      {
         rows.remove(index);
      }

      public int getColumnCount()
      {
         return 5;
      }
      public int getRowCount()
      {
         return rows.size();
      }
      public Object getValueAt(int row, int col)
      {
         return rows.get(row).get(col);
      }

      public void setValueAt(Object value, int row, int col)
      {
         // FIXME: Fire an event here
      }
      
   }
   
   
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
   }
   public Component getAWTComponent()
   {
      return scrollPane;
   }

   public void insertChildBefore(ReactComponent child, ReactComponent sibling)
   {
      super.insertChildBefore(child, sibling);
      if (child instanceof Row)
      {
         if (sibling == null)
            model.addRow((Row)child);
         else
         {
            int index = children.indexOf(sibling);
            model.insertRowAt((Row)child, index);
         }
      }
   }

   public void removeChild(ReactComponent child)
   {
      super.removeChild(child);
      if (child instanceof Row)
      {
         try
         {
            model.removeRow((Row)child);
         }
         finally
         {           
         }
      }
   }

   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      if (newChild instanceof Row)
      {
         try
         {
            int index = children.indexOf(oldChild);
            model.removeRowAt(index);
            model.insertRowAt((Row)newChild, index);
         }
         finally
         {
         }
      }
      super.replaceChild(newChild, oldChild);
   }
   
}
