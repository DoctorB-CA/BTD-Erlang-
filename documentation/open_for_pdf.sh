#!/bin/bash

# BTD-Erlang Documentation PDF Generator Helper Script

echo "🚀 BTD-Erlang Documentation PDF Generator"
echo "=========================================="
echo ""

HTML_FILE="BTD_Erlang_Project_Documentation.html"

if [ ! -f "$HTML_FILE" ]; then
    echo "❌ Error: $HTML_FILE not found!"
    echo "Run: python3 convert_to_pdf.py first"
    exit 1
fi

echo "✅ Found: $HTML_FILE"
echo ""
echo "📖 Opening HTML file for PDF conversion..."

# Try to open with different browsers
if command -v firefox >/dev/null 2>&1; then
    echo "🌐 Opening with Firefox..."
    firefox "$HTML_FILE" 2>/dev/null &
elif command -v google-chrome >/dev/null 2>&1; then
    echo "🌐 Opening with Chrome..."
    google-chrome "$HTML_FILE" 2>/dev/null &
elif command -v chromium-browser >/dev/null 2>&1; then
    echo "🌐 Opening with Chromium..."
    chromium-browser "$HTML_FILE" 2>/dev/null &
elif command -v xdg-open >/dev/null 2>&1; then
    echo "🌐 Opening with default browser..."
    xdg-open "$HTML_FILE" &
else
    echo "🌐 No browser found. Please open manually:"
    echo "   file://$(pwd)/$HTML_FILE"
fi

echo ""
echo "📋 PDF Conversion Steps:"
echo "1. ⌨️  Press Ctrl+P (Print)"
echo "2. 🎯 Destination: 'Save as PDF'"
echo "3. ⚙️  More settings:"
echo "   • Layout: Portrait"
echo "   • Margins: Minimum"
echo "   • Background graphics: Checked"
echo "4. 💾 Save as: BTD_Erlang_Project_Documentation.pdf"
echo ""
echo "🌐 Online alternatives:"
echo "   • https://html-pdf-converter.com/"
echo "   • https://www.sejda.com/html-to-pdf"
echo "   • https://smallpdf.com/html-to-pdf"
echo ""
echo "✨ Your professional PDF will be ready!"
