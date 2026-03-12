import streamlit as st
import random

# 1. Funkcja generująca wskazaną liczbę talii
def generate_deck(num_decks=1):
    suits = ['♠', '♥', '♦', '♣']
    ranks = ['A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K']
    deck = []
    # Tworzymy tyle talii, ile zażyczył sobie gracz
    for _ in range(num_decks):
        for suit in suits:
            for rank in ranks:
                value = 1 if rank == 'A' else (10 if rank in ['J', 'Q', 'K'] else int(rank))
                deck.append({'name': f"{rank}{suit}", 'value': value})
    return deck

# 2. Inicjalizacja stanu sesji (domyślnie 1 talia na start)
if 'deck' not in st.session_state:
    st.session_state.deck = generate_deck(1)
    random.shuffle(st.session_state.deck)

# 3. Funkcja symulująca skoki
def get_final_card_index(start_index, deck):
    current_idx = start_index
    while True:
        jump = deck[current_idx]['value']
        next_idx = current_idx + jump
        if next_idx >= len(deck):
            return current_idx
        current_idx = next_idx

# --- INTERFEJS UŻYTKOWNIKA ---

st.title("🎩 Sztuczka Karciana: Niewidzialna Więź")
st.markdown("Zanim zaczniemy, przygotuj karty po swojemu. Masz pełną kontrolę nad ilością kart i ich ułożeniem.")

# --- SEKCJA 1: KONTROLA GRACZA ---
st.subheader("🛠️ Krok 1: Przygotuj talię")

col1, col2 = st.columns(2)

with col1:
    num_decks = st.slider("Z ilu talii chcesz zagrać?", 1, 5, 1)
    shuffle_count = st.slider("Ile razy potasować karty?", 1, 10, 3)
    
    if st.button("🔀 Przygotuj i potasuj"):
        st.session_state.deck = generate_deck(num_decks)
        for _ in range(shuffle_count):
            random.shuffle(st.session_state.deck)
        st.success(f"Gramy {num_decks} talią/taliami ({len(st.session_state.deck)} kart). Potasowano {shuffle_count} razy!")

with col2:
    # Upewniamy się, że suwak cięcia nie wykracza poza aktualną wielkość połączonych talii
    max_cut = len(st.session_state.deck) - 1
    cut_index = st.slider("Przełóż karty (wybierz miejsce cięcia):", 1, max_cut, max_cut // 2)
    
    if st.button("✂️ Przełóż talię"):
        st.session_state.deck = st.session_state.deck[cut_index:] + st.session_state.deck[:cut_index]
        st.success("Karty przełożone!")

st.divider()

# --- SEKCJA 2: GRA ---
st.subheader("🃏 Krok 2: Twoja gra")
st.markdown("""
1. Spójrz na wyłożone poniżej karty.
2. **Wybierz w myślach** jedną z pierwszych 10 kart. Nikomu jej nie zdradzaj!
3. Przeskakuj o wartość tej karty do przodu (As=1, Figury=10, liczby = ich wartość).
4. Powtarzaj to, aż skończą się karty, i zapamiętaj tę ostatnią.
""")

# Wyświetlanie talii (bez pytania o kartę startową!)
deck_display = ""
for i, card in enumerate(st.session_state.deck):
    color = "red" if '♥' in card['name'] or '♦' in card['name'] else "black"
    deck_display += f"<span style='color:{color}; font-weight:bold; padding: 5px;'>{card['name']}</span> "
st.markdown(deck_display, unsafe_allow_html=True)

st.divider()

# --- SEKCJA 3: FINAŁ ---
st.subheader("🔮 Krok 3: Czas na magię")
st.write("Nie wiem, od jakiej karty zacząłeś. Nie wiem, jak skakałeś. Ale skup się na swojej ostatniej karcie...")

if st.button("✨ Odkryj moją przepowiednię!"):
    # Magik zawsze zaczyna od indeksu 0 (pierwsza wyłożona karta)
    magician_final_idx = get_final_card_index(0, st.session_state.deck)
    magician_card = st.session_state.deck[magician_final_idx]
    
    color = "red" if '♥' in magician_card['name'] or '♦' in magician_card['name'] else "black"
    
    st.success("Moja intuicja (i matematyka) podpowiada mi, że Twoja karta to:")
    st.markdown(f"<h1 style='text-align: center; color: {color};'>{magician_card['name']}</h1>", unsafe_allow_html=True)
    st.caption(f"(Karta na pozycji {magician_final_idx + 1})")