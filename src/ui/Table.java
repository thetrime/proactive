package org.proactive.ui;

// see http://stackoverflow.com/questions/12060587/how-to-scroll-two-or-more-jtables-with-a-single-scrollbar

import javax.swing.JTable;
import javax.swing.JScrollPane;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.JComponent;
import javax.swing.AbstractButton;
import javax.swing.JPanel;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseEvent;
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
   private ReactHeaderRenderer currentlyArmedHeader = null;
   private PopupMenu popup = null;

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
      table.getTableHeader().addMouseMotionListener(new MouseMotionAdapter()
         {
            public void mouseMoved(MouseEvent me)
            {
               int i = table.getTableHeader().columnAtPoint(me.getPoint());
               TableCellRenderer renderer = table.getColumnModel().getColumn(i).getHeaderRenderer();
               if (currentlyArmedHeader == renderer)
                  return;
               if (currentlyArmedHeader != null)
                  currentlyArmedHeader.setArmed(false);
               if (renderer instanceof ReactHeaderRenderer)
               {
                  ((ReactHeaderRenderer)renderer).setArmed(true);
                  currentlyArmedHeader = (ReactHeaderRenderer)renderer;
                  table.getTableHeader().repaint();
               }
            }
         });
      table.getTableHeader().addMouseListener(new MouseAdapter()
         {
            public void mouseExited(MouseEvent me)
            {
               if (currentlyArmedHeader != null)
                  currentlyArmedHeader.setArmed(false);
               currentlyArmedHeader = null;
            }
            public void mousePressed(MouseEvent me)
            {
               int i = table.getTableHeader().columnAtPoint(me.getPoint());
               TableCellRenderer renderer = table.getColumnModel().getColumn(i).getHeaderRenderer();
               if (renderer instanceof ReactHeaderRenderer)
               {
                  ((ReactHeaderRenderer)renderer).setActive(true);
                  table.getTableHeader().repaint();
               }
            }
            public void mouseReleased(MouseEvent me)
            {
               int i = table.getTableHeader().columnAtPoint(me.getPoint());
               TableCellRenderer renderer = table.getColumnModel().getColumn(i).getHeaderRenderer();
               if (renderer instanceof ReactHeaderRenderer)
               {
                  ((ReactHeaderRenderer)renderer).setActive(false);
                  table.getTableHeader().repaint();
               }
            }
         });
      scrollPane = new JScrollPane(table);
   }

    private MouseListener internalPopupListener = null;
   public void setContextMenuRenderer(PrologObject value)
   {
      if (internalPopupListener != null)
         table.removeMouseListener(internalPopupListener);
      if (value.isNull())
         return;
      internalPopupListener = new MouseAdapter()
         {
            public void createPopup(MouseEvent me)
            {
               try
               {
                  HashMap<String, Object> event = new HashMap<String, Object>();
                  event.put("row_number", table.rowAtPoint(me.getPoint()));
                  popup = (PopupMenu)getOwnerDocument().renderContextualElement(value.asTerm(), PrologObject.serialize(event).asTerm());
                  popup.getMenu().show(table, me.getX(), me.getY());
               }
               catch(Exception e)
               {
                  e.printStackTrace();
               }
            }
            public void mouseClicked(MouseEvent me)
            {
               if (me.isPopupTrigger())
               {
                  createPopup(me);
               }
            }
            public void mousePressed(MouseEvent me)
            {
               if (me.isPopupTrigger())
               {
                  createPopup(me);
               }
            }
         };
      table.addMouseListener(internalPopupListener);
   }
   
   public class ReactTableCellRenderer implements TableCellRenderer
   {
      private JPanel nullPanel = null;
      public ReactTableCellRenderer()
      {
         nullPanel = new JPanel();
      }
      
      public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column)
      {
         Component c = null;
         if (value == null)
            c = nullPanel;
         else
            c = ((ReactComponent)value).getAWTComponent();
         if (c instanceof JComponent)
            ((JComponent)c).setOpaque(isSelected);
         if (isSelected)
            c.setBackground(java.awt.Color.LIGHT_GRAY);

         return c;
      }
   }

   public class ReactHeaderRenderer implements TableCellRenderer
   {
      private ReactComponent component;
      private boolean isActive = false;
      private boolean isArmed = false;
      public ReactHeaderRenderer(ReactComponent component)
      {
         this.component = component;
      }
      public void setActive(boolean b)
      {
         isActive = b;
      }
      public void setArmed(boolean b)
      {
         isArmed = b;
      }
      public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column)
      {
         Component c = component.getAWTComponent();
         if (c instanceof AbstractButton)
         {
            ((AbstractButton)c).getModel().setPressed(isActive);
            ((AbstractButton)c).getModel().setArmed(isArmed);
         }
         return c;
      }
   }

   public class ReactTableModel extends AbstractTableModel implements RowChangeListener
   {
      protected LinkedList<Row> rows = new LinkedList<Row>();
      int columnCount = 1;

      public void rowChanged(Row row)
      {
	 int index = rows.indexOf(row);
	 fireTableRowsUpdated(index, index);
      }

      public void addRow(Row row)
      {
         if (row.getSize() > columnCount)
         {
            columnCount = row.getSize();
            fireTableStructureChanged();
         }
	 rows.add(row);
	 row.addRowChangeListener(this);
	 fireTableRowsInserted(rows.size(), rows.size());
      }

      public void insertRowAt(Row row, int index)
      {
         if (row.getSize() > columnCount)
         {
            columnCount = row.getSize();
            fireTableStructureChanged();
         }
	 rows.add(index, row);
	 row.addRowChangeListener(this);
	 fireTableRowsInserted(index, index);
      }

      public void removeRow(Row row)
      {
	 // We do not shrink width of the table here. Perhaps we should...
	 int index = model.indexOfRow(row);
	 rows.remove(row);
	 row.removeRowChangeListener(this);
	 fireTableRowsDeleted(index, index);
      }

      public void removeRowAt(int index)
      {
	 // We do not shrink the width of table here. Perhaps we should...
	 Row row = rows.get(index);
	 row.removeRowChangeListener(this);
	 rows.remove(index);
	 fireTableRowsDeleted(index, index);
      }

      public int getColumnCount()
      {
         return columnCount;
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

      public int indexOfRow(Row row)
      {
         return rows.indexOf(row);
      }
      public void setHeader(TableHeader header)
      {
         int i = 0;
         if (header.getSize() > columnCount)
            columnCount = header.getSize();
         fireTableStructureChanged();
         for (ReactComponent o : header.getColumnHeaders())
         {
            table.getColumnModel().getColumn(i).setHeaderRenderer(new ReactHeaderRenderer(o));
            i++;
         }
      }

   }
   
   
   public void setProperties(HashMap<String, PrologObject> properties)
   {
      super.setProperties(properties);
      if (properties.containsKey("renderContextMenu"))
         setContextMenuRenderer(properties.get("renderContextMenu"));

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
            // FIXME: sibling may not be a row!
            int index = model.indexOfRow((Row)sibling);
            model.insertRowAt((Row)child, index);
         }
      }
      else if (child instanceof TableHeader)
      {
         // It doesnt matter (currently) where the header is in the table. Later this might change
         model.setHeader((TableHeader)child);
      }
   }

   public void removeChild(ReactComponent child)
   {
      super.removeChild(child);
      if (child instanceof Row)
      {
         model.removeRow((Row)child);
      }
      else if (child instanceof TableHeader)
      {
         // This means there is no longer to be any header?
      }

   }

   public void replaceChild(ReactComponent newChild, ReactComponent oldChild)
   {
      int index = -1;
      if (oldChild instanceof Row)
      {
         index = model.indexOfRow((Row)oldChild);
         model.removeRowAt(index);
      }
      else if (oldChild instanceof TableHeader)
      {
         //
      }

      if (newChild instanceof Row)
      {
         if (index == -1)
            model.addRow((Row)newChild);
         else
            model.insertRowAt((Row)newChild, index);
      }
      else if (newChild instanceof TableHeader)
      {
         //
      }

      super.replaceChild(newChild, oldChild);
   }
   
}
