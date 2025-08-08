#!/usr/bin/env python2
# -*- coding: utf-8 -*-
##################################################
# GNU Radio Python Flow Graph
# Title: Top Block
# GNU Radio version: 3.7.14.0
##################################################

if __name__ == '__main__':
    import ctypes
    import sys
    if sys.platform.startswith('linux'):
        try:
            x11 = ctypes.cdll.LoadLibrary('libX11.so')
            x11.XInitThreads()
        except:
            print "Warning: failed to XInitThreads()"

from PyQt4 import Qt
from gnuradio import blocks
from gnuradio import eng_notation
from gnuradio import filter
from gnuradio import gr
from gnuradio import qtgui
from gnuradio.eng_option import eng_option
from gnuradio.filter import firdes
from gnuradio.qtgui import Range, RangeWidget
from optparse import OptionParser
import sip
import sys
from gnuradio import qtgui


class top_block(gr.top_block, Qt.QWidget):

    def __init__(self):
        gr.top_block.__init__(self, "Top Block")
        Qt.QWidget.__init__(self)
        self.setWindowTitle("Top Block")
        qtgui.util.check_set_qss()
        try:
            self.setWindowIcon(Qt.QIcon.fromTheme('gnuradio-grc'))
        except:
            pass
        self.top_scroll_layout = Qt.QVBoxLayout()
        self.setLayout(self.top_scroll_layout)
        self.top_scroll = Qt.QScrollArea()
        self.top_scroll.setFrameStyle(Qt.QFrame.NoFrame)
        self.top_scroll_layout.addWidget(self.top_scroll)
        self.top_scroll.setWidgetResizable(True)
        self.top_widget = Qt.QWidget()
        self.top_scroll.setWidget(self.top_widget)
        self.top_layout = Qt.QVBoxLayout(self.top_widget)
        self.top_grid_layout = Qt.QGridLayout()
        self.top_layout.addLayout(self.top_grid_layout)

        self.settings = Qt.QSettings("GNU Radio", "top_block")
        self.restoreGeometry(self.settings.value("geometry").toByteArray())


        ##################################################
        # Variables
        ##################################################
        self.symbol_guard = symbol_guard = 5
        self.samp_rate = samp_rate = 32000
        self.delay_spread = delay_spread = 1

        ##################################################
        # Blocks
        ##################################################
        self._symbol_guard_range = Range(0, 200, 1, 5, 200)
        self._symbol_guard_win = RangeWidget(self._symbol_guard_range, self.set_symbol_guard, "symbol_guard", "counter_slider", int)
        self.top_grid_layout.addWidget(self._symbol_guard_win)
        self._delay_spread_range = Range(0, 50, 1, 1, 200)
        self._delay_spread_win = RangeWidget(self._delay_spread_range, self.set_delay_spread, "delay_spread", "counter_slider", int)
        self.top_grid_layout.addWidget(self._delay_spread_win)
        self.ray = qtgui.time_sink_f(
        	1024, #size
        	samp_rate, #samp_rate
        	'Rays', #name
        	10 #number of inputs
        )
        self.ray.set_update_time(0.10)
        self.ray.set_y_axis(-1, 1)

        self.ray.set_y_label('Amplitude', "")

        self.ray.enable_tags(-1, True)
        self.ray.set_trigger_mode(qtgui.TRIG_MODE_FREE, qtgui.TRIG_SLOPE_POS, 0.0, 0, 0, "")
        self.ray.enable_autoscale(False)
        self.ray.enable_grid(False)
        self.ray.enable_axis_labels(True)
        self.ray.enable_control_panel(True)
        self.ray.enable_stem_plot(False)

        if not True:
          self.ray.disable_legend()

        labels = ['Ray 1', 'Ray 2', 'Ray 3', 'Ray 4 ', 'Ray 5',
                  'Ray 6', 'Ray 7', 'Ray 8', 'Ray 9', 'Combined Ray']
        widths = [1, 1, 1, 1, 1,
                  1, 1, 1, 1, 1]
        colors = ["blue", "red", "green", "black", "cyan",
                  "magenta", "yellow", "dark red", "dark green", "Dark Blue"]
        styles = [1, 1, 1, 1, 1,
                  1, 1, 1, 1, 5]
        markers = [-1, -1, -1, -1, -1,
                   -1, -1, -1, -1, 0]
        alphas = [1.0, 1.0, 1.0, 1.0, 1.0,
                  1.0, 1.0, 1.0, 1.0, 1.0]

        for i in xrange(10):
            if len(labels[i]) == 0:
                self.ray.set_line_label(i, "Data {0}".format(i))
            else:
                self.ray.set_line_label(i, labels[i])
            self.ray.set_line_width(i, widths[i])
            self.ray.set_line_color(i, colors[i])
            self.ray.set_line_style(i, styles[i])
            self.ray.set_line_marker(i, markers[i])
            self.ray.set_line_alpha(i, alphas[i])

        self._ray_win = sip.wrapinstance(self.ray.pyqwidget(), Qt.QWidget)
        self.top_grid_layout.addWidget(self._ray_win)
        self.interp_fir_filter_xxx_0 = filter.interp_fir_filter_fff(4, (firdes.root_raised_cosine(4,4,1,0.25,4*45)))
        self.interp_fir_filter_xxx_0.declare_sample_delay(0)
        self.blocks_multiply_const_vxx_1 = blocks.multiply_const_vff((0.1111111, ))
        self.blocks_multiply_const_vxx_0_3 = blocks.multiply_const_vff((-1, ))
        self.blocks_multiply_const_vxx_0_0 = blocks.multiply_const_vff((-1, ))
        self.blocks_delay_1_7 = blocks.delay(gr.sizeof_float*1, delay_spread*2)
        self.blocks_delay_1_6 = blocks.delay(gr.sizeof_float*1, delay_spread*3)
        self.blocks_delay_1_5 = blocks.delay(gr.sizeof_float*1, delay_spread*4)
        self.blocks_delay_1_4 = blocks.delay(gr.sizeof_float*1, delay_spread*5)
        self.blocks_delay_1_3 = blocks.delay(gr.sizeof_float*1, delay_spread*6)
        self.blocks_delay_1_2 = blocks.delay(gr.sizeof_float*1, delay_spread*7)
        self.blocks_delay_1_1 = blocks.delay(gr.sizeof_float*1, delay_spread*8)
        self.blocks_delay_1 = blocks.delay(gr.sizeof_float*1, delay_spread)
        self.blocks_delay_0_3 = blocks.delay(gr.sizeof_float*1, symbol_guard*5)
        self.blocks_delay_0_2 = blocks.delay(gr.sizeof_float*1, symbol_guard*4)
        self.blocks_delay_0_1 = blocks.delay(gr.sizeof_float*1, symbol_guard*2)
        self.blocks_delay_0_0 = blocks.delay(gr.sizeof_float*1, symbol_guard*3)
        self.blocks_delay_0 = blocks.delay(gr.sizeof_float*1, symbol_guard)
        self.blocks_add_xx_0_0 = blocks.add_vff(1)
        self.blocks_add_xx_0 = blocks.add_vff(1)
        self.Vector_src = blocks.vector_source_f(31*[0,] + [1,] + 32*[0,], True, 1, [])
        self.Symbol = qtgui.time_sink_f(
        	1024, #size
        	samp_rate, #samp_rate
        	'Symbol sequence', #name
        	7 #number of inputs
        )
        self.Symbol.set_update_time(0.10)
        self.Symbol.set_y_axis(-1, 1)

        self.Symbol.set_y_label('Amplitude', "")

        self.Symbol.enable_tags(-1, False)
        self.Symbol.set_trigger_mode(qtgui.TRIG_MODE_FREE, qtgui.TRIG_SLOPE_POS, 0.0, 0, 0, "")
        self.Symbol.enable_autoscale(False)
        self.Symbol.enable_grid(False)
        self.Symbol.enable_axis_labels(True)
        self.Symbol.enable_control_panel(True)
        self.Symbol.enable_stem_plot(False)

        if not True:
          self.Symbol.disable_legend()

        labels = ['Combined Symbol', ' Symbol1', ' Symbol2', ' Symbol 3', ' Symbol 4',
                  ' Symbol 5', ' Symbol 6', '', '', '']
        widths = [1, 1, 1, 1, 1,
                  1, 1, 1, 1, 1]
        colors = ["blue", "red", "green", "black", "cyan",
                  "magenta", "yellow", "dark red", "dark green", "blue"]
        styles = [5, 1, 1, 1, 2,
                  1, 1, 1, 1, 1]
        markers = [0, -1, -1, -1, -1,
                   -1, -1, -1, -1, -1]
        alphas = [1.0, 1.0, 1.0, 1.0, 1.0,
                  1.0, 1.0, 1.0, 1.0, 1.0]

        for i in xrange(7):
            if len(labels[i]) == 0:
                self.Symbol.set_line_label(i, "Data {0}".format(i))
            else:
                self.Symbol.set_line_label(i, labels[i])
            self.Symbol.set_line_width(i, widths[i])
            self.Symbol.set_line_color(i, colors[i])
            self.Symbol.set_line_style(i, styles[i])
            self.Symbol.set_line_marker(i, markers[i])
            self.Symbol.set_line_alpha(i, alphas[i])

        self._Symbol_win = sip.wrapinstance(self.Symbol.pyqwidget(), Qt.QWidget)
        self.top_grid_layout.addWidget(self._Symbol_win)



        ##################################################
        # Connections
        ##################################################
        self.connect((self.Vector_src, 0), (self.interp_fir_filter_xxx_0, 0))
        self.connect((self.blocks_add_xx_0, 0), (self.Symbol, 0))
        self.connect((self.blocks_add_xx_0, 0), (self.blocks_add_xx_0_0, 0))
        self.connect((self.blocks_add_xx_0, 0), (self.blocks_delay_1, 0))
        self.connect((self.blocks_add_xx_0, 0), (self.blocks_delay_1_1, 0))
        self.connect((self.blocks_add_xx_0, 0), (self.blocks_delay_1_2, 0))
        self.connect((self.blocks_add_xx_0, 0), (self.blocks_delay_1_3, 0))
        self.connect((self.blocks_add_xx_0, 0), (self.blocks_delay_1_4, 0))
        self.connect((self.blocks_add_xx_0, 0), (self.blocks_delay_1_5, 0))
        self.connect((self.blocks_add_xx_0, 0), (self.blocks_delay_1_6, 0))
        self.connect((self.blocks_add_xx_0, 0), (self.blocks_delay_1_7, 0))
        self.connect((self.blocks_add_xx_0, 0), (self.ray, 0))
        self.connect((self.blocks_add_xx_0_0, 0), (self.blocks_multiply_const_vxx_1, 0))
        self.connect((self.blocks_delay_0, 0), (self.Symbol, 2))
        self.connect((self.blocks_delay_0, 0), (self.blocks_add_xx_0, 1))
        self.connect((self.blocks_delay_0_0, 0), (self.Symbol, 4))
        self.connect((self.blocks_delay_0_0, 0), (self.blocks_add_xx_0, 3))
        self.connect((self.blocks_delay_0_1, 0), (self.Symbol, 3))
        self.connect((self.blocks_delay_0_1, 0), (self.blocks_add_xx_0, 2))
        self.connect((self.blocks_delay_0_2, 0), (self.Symbol, 5))
        self.connect((self.blocks_delay_0_2, 0), (self.blocks_add_xx_0, 4))
        self.connect((self.blocks_delay_0_3, 0), (self.Symbol, 6))
        self.connect((self.blocks_delay_0_3, 0), (self.blocks_add_xx_0, 5))
        self.connect((self.blocks_delay_1, 0), (self.blocks_add_xx_0_0, 1))
        self.connect((self.blocks_delay_1, 0), (self.ray, 1))
        self.connect((self.blocks_delay_1_1, 0), (self.blocks_add_xx_0_0, 8))
        self.connect((self.blocks_delay_1_1, 0), (self.ray, 8))
        self.connect((self.blocks_delay_1_2, 0), (self.blocks_add_xx_0_0, 7))
        self.connect((self.blocks_delay_1_2, 0), (self.ray, 7))
        self.connect((self.blocks_delay_1_3, 0), (self.blocks_add_xx_0_0, 6))
        self.connect((self.blocks_delay_1_3, 0), (self.ray, 6))
        self.connect((self.blocks_delay_1_4, 0), (self.blocks_add_xx_0_0, 5))
        self.connect((self.blocks_delay_1_4, 0), (self.ray, 5))
        self.connect((self.blocks_delay_1_5, 0), (self.blocks_add_xx_0_0, 4))
        self.connect((self.blocks_delay_1_5, 0), (self.ray, 4))
        self.connect((self.blocks_delay_1_6, 0), (self.blocks_add_xx_0_0, 3))
        self.connect((self.blocks_delay_1_6, 0), (self.ray, 3))
        self.connect((self.blocks_delay_1_7, 0), (self.blocks_add_xx_0_0, 2))
        self.connect((self.blocks_delay_1_7, 0), (self.ray, 2))
        self.connect((self.blocks_multiply_const_vxx_0_0, 0), (self.blocks_delay_0, 0))
        self.connect((self.blocks_multiply_const_vxx_0_3, 0), (self.blocks_delay_0_0, 0))
        self.connect((self.blocks_multiply_const_vxx_1, 0), (self.ray, 9))
        self.connect((self.interp_fir_filter_xxx_0, 0), (self.Symbol, 1))
        self.connect((self.interp_fir_filter_xxx_0, 0), (self.blocks_add_xx_0, 0))
        self.connect((self.interp_fir_filter_xxx_0, 0), (self.blocks_delay_0_1, 0))
        self.connect((self.interp_fir_filter_xxx_0, 0), (self.blocks_delay_0_2, 0))
        self.connect((self.interp_fir_filter_xxx_0, 0), (self.blocks_delay_0_3, 0))
        self.connect((self.interp_fir_filter_xxx_0, 0), (self.blocks_multiply_const_vxx_0_0, 0))
        self.connect((self.interp_fir_filter_xxx_0, 0), (self.blocks_multiply_const_vxx_0_3, 0))

    def closeEvent(self, event):
        self.settings = Qt.QSettings("GNU Radio", "top_block")
        self.settings.setValue("geometry", self.saveGeometry())
        event.accept()

    def get_symbol_guard(self):
        return self.symbol_guard

    def set_symbol_guard(self, symbol_guard):
        self.symbol_guard = symbol_guard
        self.blocks_delay_0_3.set_dly(self.symbol_guard*5)
        self.blocks_delay_0_2.set_dly(self.symbol_guard*4)
        self.blocks_delay_0_1.set_dly(self.symbol_guard*2)
        self.blocks_delay_0_0.set_dly(self.symbol_guard*3)
        self.blocks_delay_0.set_dly(self.symbol_guard)

    def get_samp_rate(self):
        return self.samp_rate

    def set_samp_rate(self, samp_rate):
        self.samp_rate = samp_rate
        self.ray.set_samp_rate(self.samp_rate)
        self.Symbol.set_samp_rate(self.samp_rate)

    def get_delay_spread(self):
        return self.delay_spread

    def set_delay_spread(self, delay_spread):
        self.delay_spread = delay_spread
        self.blocks_delay_1_7.set_dly(self.delay_spread*2)
        self.blocks_delay_1_6.set_dly(self.delay_spread*3)
        self.blocks_delay_1_5.set_dly(self.delay_spread*4)
        self.blocks_delay_1_4.set_dly(self.delay_spread*5)
        self.blocks_delay_1_3.set_dly(self.delay_spread*6)
        self.blocks_delay_1_2.set_dly(self.delay_spread*7)
        self.blocks_delay_1_1.set_dly(self.delay_spread*8)
        self.blocks_delay_1.set_dly(self.delay_spread)


def main(top_block_cls=top_block, options=None):

    from distutils.version import StrictVersion
    if StrictVersion(Qt.qVersion()) >= StrictVersion("4.5.0"):
        style = gr.prefs().get_string('qtgui', 'style', 'raster')
        Qt.QApplication.setGraphicsSystem(style)
    qapp = Qt.QApplication(sys.argv)

    tb = top_block_cls()
    tb.start()
    tb.show()

    def quitting():
        tb.stop()
        tb.wait()
    qapp.connect(qapp, Qt.SIGNAL("aboutToQuit()"), quitting)
    qapp.exec_()


if __name__ == '__main__':
    main()
