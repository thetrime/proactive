package org.proactive.ui;

// see http://stackoverflow.com/questions/12060587/how-to-scroll-two-or-more-jtables-with-a-single-scrollbar

import javax.swing.JTable;
import javax.swing.JScrollPane;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.event.TableColumnModelListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.ChangeEvent;
import javax.swing.JComponent;
import javax.swing.AbstractButton;
import javax.swing.JPanel;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseEvent;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.util.List;
import java.util.LinkedList;
import java.util.HashMap;
import java.awt.Component;
import org.proactive.prolog.PrologObject;
import org.proactive.prolog.Engine;
import org.proactive.ui.ProactiveConstraints;
import org.proactive.ReactComponent;

public class Table extends ReactComponent  implements TableColumnModelListener
{
   JTable table;
   JTable footer;
   JPanel panel;
   JScrollPane scrollPane;
   TableFooter footerElement = null;
   ReactTableModel model = new ReactTableModel();
   TableCellRenderer cellRenderer = new ReactTableCellRenderer();
   private ReactHeaderRenderer currentlyArmedHeader = null;
   private PopupMenu popup = null;

   public Table()
   {
      panel = new JPanel()
         {
            public Dimension getMinimumSize()
            {
               return new Dimension((int)getPreferredSize().getWidth(), 0);
            }
            public Dimension getPreferredSize()
            {
               return table.getPreferredSize();
            }

         };
      cellRenderer = new ReactTableCellRenderer();
      table = new JTable(model)
         {
            public TableCellRenderer getCellRenderer(int row, int column)
            {
               return cellRenderer;
            }
         };
      table.setPreferredScrollableViewportSize(table.getPreferredSize());
      table.setFillsViewportHeight(true);
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
      table.addMouseListener(new MouseAdapter()
	 {
	    public void mouseClicked(MouseEvent me)
	    {
	       if (me.getClickCount() == 2)
	       {
		  int index = table.rowAtPoint(me.getPoint());
		  if (index != -1)
		  {
		     Row row = model.getRowAt(index);
		     row.doubleClick();
		     // FIXME: Cells may also have double-click handlers!
		  }
	       }
	    }
	 });
      scrollPane = new JScrollPane(table);
      panel.setLayout(new BorderLayout());
      panel.add(scrollPane, BorderLayout.CENTER);
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

   public void columnRemoved(TableColumnModelEvent e) {}
   public void columnMoved(TableColumnModelEvent e) {}
   public void columnAdded(TableColumnModelEvent e) {}
   public void columnSelectionChanged(ListSelectionEvent e) {}
   public void columnMarginChanged(ChangeEvent event)
   {
      final TableColumnModel eventModel = (DefaultTableColumnModel)event.getSource();
      final TableColumnModel thisModel = table.getColumnModel();
      final int columnCount = eventModel.getColumnCount();
      for (int i = 0; i < columnCount; i++)
      {
         thisModel.getColumn(i).setWidth(eventModel.getColumn(i).getWidth());
      }
      table.repaint();
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

      public Row getRowAt(int row)
      {
	 return rows.get(row);
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
         int w = 0;
         for (ReactComponent o : header.getColumnHeaders())
            if (o.getWeight() > 1)
               w+=o.getWeight();
         double factor = ((double)table.getPreferredSize().width * 10) / (double)w;
         for (ReactComponent o : header.getColumnHeaders())
         {
            table.getColumnModel().getColumn(i).setHeaderRenderer(new ReactHeaderRenderer(o));
            if (o.getWeight() != -1)
            {
               table.getColumnModel().getColumn(i).setPreferredWidth((int)(factor*o.getWeight()));
            }
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
      return panel;
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
      else if (child instanceof TableFooter)
      {
         createFooter((TableFooter)child);
      }
      configureViewport();
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
      else if (child instanceof TableFooter)
      {
         // In theory you could add more than one footer. We only remove the footer if it is the current one.
         if (child == footerElement)
         {
            footerElement = null;
            panel.remove(footer);
         }

      }

      configureViewport();
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
      else if (oldChild instanceof TableFooter)
      {
         // In theory you could add more than one footer. We only remove the footer if it is the current one.
         if (oldChild == footerElement)
         {
            footerElement = null;
            panel.remove(footer);
         }
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
      else if (newChild instanceof TableFooter)
      {
         createFooter((TableFooter)newChild);
      }

      configureViewport();
      super.replaceChild(newChild, oldChild);
   }

   private void createFooter(TableFooter f)
   {
      footerElement = f;
      footer = new JTable(new FooterModel(footerElement))
         {
            public TableCellRenderer getCellRenderer(int row, int column)
            {
               return cellRenderer;
            }
         };
      footer.setColumnModel(table.getColumnModel());
      TableColumnModelListener footerListener = new TableColumnModelListener()
         {
            public void columnRemoved(TableColumnModelEvent e) {}
            public void columnMoved(TableColumnModelEvent e) {}
            public void columnAdded(TableColumnModelEvent e) {}
            public void columnSelectionChanged(ListSelectionEvent e) {}
            public void columnMarginChanged(ChangeEvent event)
            {
               final TableColumnModel eventModel = (DefaultTableColumnModel)event.getSource();
               final TableColumnModel thisModel = footer.getColumnModel();
               final int columnCount = eventModel.getColumnCount();
               for (int i = 0; i < columnCount; i++)
               {
                  thisModel.getColumn(i).setWidth(eventModel.getColumn(i).getWidth());
               }
               footer.repaint();
            }
         };
      table.getColumnModel().addColumnModelListener(footerListener);
      footer.getColumnModel().addColumnModelListener(this);
      panel.add(footer, BorderLayout.SOUTH);
   }

   public ProactiveConstraints.Fill getFill()
   {
      if (super.getFill() == ProactiveConstraints.Fill.NONE)
         return ProactiveConstraints.Fill.HORIZONTAL;
      return super.getFill();
   }


   private void configureViewport()
   {
      //table.setPreferredScrollableViewportSize(new Dimension(table.getWidth(), table.getRowCount() * (1+table.getRowHeight())));
      table.setPreferredScrollableViewportSize(table.getPreferredSize());
   }

   public class FooterModel extends AbstractTableModel
   {
      TableFooter footer;
      public FooterModel(TableFooter f)
      {
         // FIXME: Need to add a listener to the footer here. When it changes, trigger appropriate table events to
         // let Java know that the number of rows, columns, and values may have changed.
         this.footer = f;
      }
      public int getRowCount()
      {
         return footer.getRowCount();
      }
      public int getColumnCount()
      {
         // Same number of columns as main table
         return model.getColumnCount();
      }
      public Object getValueAt(int row, int column)
      {
         Row r = (Row)footer.getRow(row);
         if (r == null)
            return null;
         return r.get(column);
      }
   }

}
